namespace Fable.Remoting.AzureFunctionV2

open Fable.Remoting.Server
open Newtonsoft.Json
open System.Net
open System.Net.Http

module AzureFunctionV2Util = 

  // TODO Diagnostics.outputPhase logger json
  let textResponse (statusCode: HttpStatusCode) (content: string) (mimeType: string) = 
    let response = new HttpResponseMessage(statusCode)
    response.Content <- new StringContent(content, System.Text.Encoding.UTF8, mimeType)   
    response 

  let binaryResponse (statusCode: HttpStatusCode) (content: byte[]) = 
    let response = new HttpResponseMessage(statusCode)
    response.Content <- new ByteArrayContent(content)   
    response 
  
  let html content = 
    textResponse HttpStatusCode.OK content "text/html"

  /// Used to halt the forwarding of the Http context
  let halt = 
    new HttpResponseMessage(HttpStatusCode.NotFound)

  /// Sets the error object in the response and makes the status code 500 (Internal Server Error)
  let sendError error logger = 
    textResponse HttpStatusCode.InternalServerError error "application/json"

  /// Runs the given dynamic function and catches unhandled exceptions, sending them off to the configured error handler, if any. Returns 200 (OK) status code for successful runs and 500  (Internal Server Error) when an exception is thrown 
  let runFunction func impl options args = 
    let logger = options.DiagnosticsLogger
    fun (request: HttpRequestMessage) -> async {
      Diagnostics.runPhase logger func.FunctionName
      let! functionResult = Async.Catch (DynamicRecord.invokeAsync func impl args) 
      match functionResult with
      | Choice.Choice1Of2 output ->
          let isBinaryOutput = 
            match func.Type with 
            | NoArguments t when t = typeof<Async<byte[]>> -> true
            | SingleArgument (i, t) when t = typeof<Async<byte[]>> -> true
            | ManyArguments (i, t) when t = typeof<Async<byte[]>> -> true
            | otherwise -> false
             
          let isFableProxyRequest = 
            request.Headers
            |> Seq.map (fun kv -> (kv.Key, kv.Value))
            |> Map.ofSeq
            |> Map.containsKey "x-remoting-proxy" 

          if isBinaryOutput && isFableProxyRequest then 
            let binaryContent = unbox<byte[]> output
            return binaryResponse HttpStatusCode.OK binaryContent
          else
            let json = DynamicRecord.serialize output
            return textResponse  HttpStatusCode.OK json "application/json"
      | Choice.Choice2Of2 ex -> 
          return sendError "{ OK: false }" logger
    }
  
  let (|HttpGet|HttpPost|HttpOptions|Other|)  (method: HttpMethod) =
    if   method = HttpMethod.Get     then HttpGet
    elif method = HttpMethod.Post    then HttpPost
    elif method = HttpMethod.Options then HttpOptions
    else                                  Other method
    
  /// Builds the entire WebPart from implementation record, handles routing and dynamic running of record functions
  let buildFromImplementation impl options = 
    let dynamicFunctions = DynamicRecord.createRecordFuncInfo impl
    let typeName = impl.GetType().Name   
    fun (request: HttpRequestMessage) -> async {
      // todo PathAndQuery should just be the path. Not that query info should be sent up...
      let requestPath = 
        if request.RequestUri.PathAndQuery.StartsWith("/") 
        then request.RequestUri.PathAndQuery.Substring(1)
        else request.RequestUri.PathAndQuery

      let foundFunction = 
        dynamicFunctions 
        |> Map.tryFindKey (fun funcName _ -> requestPath = options.RouteBuilder typeName funcName) 
      match foundFunction with 
      | None -> 
          match request.Method, options.Docs with 
          | HttpGet, (Some docsUrl, Some docs) when docsUrl = requestPath -> 
              let (Documentation(docsName, docsRoutes)) = docs
              let schema = DynamicRecord.makeDocsSchema (impl.GetType()) docs options.RouteBuilder
              let docsApp = DocsApp.embedded docsName docsUrl schema
              return html docsApp
          | HttpOptions, (Some docsUrl, Some docs) 
                when sprintf "/%s/$schema" docsUrl = requestPath
                  || sprintf "%s/$schema" docsUrl = requestPath ->
              let schema = DynamicRecord.makeDocsSchema (impl.GetType()) docs options.RouteBuilder
              let serializedSchema =  schema.ToString(Formatting.None)
              return textResponse HttpStatusCode.OK serializedSchema "application/json"
          | _ -> 
              return halt      
      | Some funcName -> 
          let contentIsBinaryEncoded = 
            request.Headers
            |> Seq.tryFind (fun kv -> kv.Key.ToLowerInvariant() = "content-type")
            |> Option.bind (fun kv -> kv.Value |> Seq.tryHead)
            |> (=) (Some "application/octet-stream")
 
          let func = Map.find funcName dynamicFunctions
          
          match request.Method, func.Type with  
          | (HttpGet | HttpPost), NoArguments _ ->  
              return! runFunction func impl options [|  |] request
          
          | (HttpGet | HttpPost), SingleArgument(input, _) when input = typeof<unit> ->
              return! runFunction func impl options [|  |] request   

          | HttpPost, SingleArgument(input, _) when input = typeof<byte[]> && contentIsBinaryEncoded ->
              let! inputBytes = request.Content.ReadAsByteArrayAsync() |> Async.AwaitTask
              let inputArgs = [| box inputBytes |]
              return! runFunction func impl options inputArgs request
          
          | HttpPost, _ ->      
              let! inputBytes = request.Content.ReadAsByteArrayAsync() |> Async.AwaitTask
              let inputJson = System.Text.Encoding.UTF8.GetString(inputBytes)
              let inputArgs = DynamicRecord.tryCreateArgsFromJson func inputJson options.DiagnosticsLogger
              match inputArgs with 
              | Ok inputArgs -> return! runFunction func impl options inputArgs request
              | Result.Error error -> return sendError "{ OK: false }" None
          | _ -> 
              return sendError "{ OK: false }" None
    }


module Remoting = 
    open System
    open System.Threading.Tasks

    let buildFunc (options: RemotingOptions<HttpRequestMessage, 't>) : HttpRequestMessage -> Task<HttpResponseMessage> =
        match options.Implementation with
        | Empty -> 
            fun (request: HttpRequestMessage) -> Task.FromException<HttpResponseMessage>(Exception("Not found..."))

        | StaticValue impl -> 
            let webPart = AzureFunctionV2Util.buildFromImplementation impl options 

            (webPart >> Async.StartAsTask)

        | FromContext createImplementationFrom -> 
            fun (request: HttpRequestMessage) -> 
                let impl = createImplementationFrom request
                
                Task.FromException<HttpResponseMessage>(Exception("Not found..."))

    let fromContextWithState (f: HttpRequestMessage -> 'State -> 't) (options: RemotingOptions<HttpRequestMessage, 't>) : RemotingOptions<HttpRequestMessage * 'State, 't> = 
        let errorHandler: ErrorHandler<HttpRequestMessage * 'State> option =
            match options.ErrorHandler with
            | Some handler ->
                Some (fun (ex: System.Exception) (routeInfo: RouteInfo<HttpRequestMessage * 'State>) ->
                        let routeInfo' = 
                            { path        = routeInfo.path
                              methodName  = routeInfo.methodName
                              httpContext = fst routeInfo.httpContext}
                        handler ex routeInfo')

            | None -> None
            
        { Implementation    = FromContext (fun (request, state) -> f request state) 
          RouteBuilder      = options.RouteBuilder
          ErrorHandler      = errorHandler
          DiagnosticsLogger = options.DiagnosticsLogger
          Docs              = options.Docs }

