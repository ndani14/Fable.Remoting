module SuaveTester

open System.Net.Http
open System.Threading.Tasks


let runWith (webPart: HttpRequestMessage -> Task<HttpResponseMessage>) = 
    webPart

let req (methd: HttpMethod) (path: string) (data: 'a option) (webPart: HttpRequestMessage -> Task<HttpResponseMessage>) =
  let request = new HttpRequestMessage(methd, sprintf "http://localhost/%s" path)
  match data with 
  | Some content -> 
      request.Content <- content
  | None -> ()
  
  let responseTask = webPart request
  
  let bytes =
    responseTask.GetAwaiter().GetResult()
        .Content.ReadAsByteArrayAsync().GetAwaiter().GetResult()

  System.Text.Encoding.UTF8.GetString bytes

