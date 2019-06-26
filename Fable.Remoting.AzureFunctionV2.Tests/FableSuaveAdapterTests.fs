﻿module FableSuaveAdapterTests

open Fable.Remoting.Server
open Fable.Remoting.AzureFunctionV2
open Fable.Remoting.Json

open Newtonsoft.Json

open System.Net.Http
open SuaveTester

open System
open Expecto
open Types

// Test helpers

let equal x y = Expect.equal true (x = y) (sprintf "%A = %A" x y)
let pass () = Expect.equal true true ""   
let fail () = Expect.equal false true ""

let errorHandler (ex: exn) (_: RouteInfo<_>) = 
    printfn "Propagating exception message back to client: %s" ex.Message
    Propagate (ex.Message)


let app = 
  Remoting.createApi()
  |> Remoting.fromValue implementation  
  |> Remoting.withDiagnosticsLogger (printfn "%s")
  |> Remoting.withErrorHandler errorHandler 
  |> Remoting.buildFunc

let postContent (input: string) =  new StringContent(sprintf "[%s]" input, System.Text.Encoding.UTF8)
let postRaw (input: string) =  new StringContent(input, System.Text.Encoding.UTF8)

let converter : JsonConverter = FableJsonConverter() :> JsonConverter
let toJson (x: obj) = JsonConvert.SerializeObject(x, [| converter |])

let ofJson<'t> (input: string) = JsonConvert.DeserializeObject<'t>(input, [| converter |])

let fableSuaveAdapterTests = 
    testList "FableSuaveAdapter tests" [
        testCase "Sending string as input works" <| fun () ->
            let input = "\"my-test-string\"";
            let content = postContent input
            runWith app
            |> req HttpMethod.Post "/IProtocol/getLength" (Some content)
            |> fun result -> equal result "14"

        testCase "Sending int as input works" <| fun () ->
            let input = postContent "5" 
            runWith app
            |> req HttpMethod.Post "/IProtocol/echoInteger" (Some input)
            |> fun result -> equal "10" result

        testCase "DateTimeOffset roundtrip" <| fun () -> 
            let input = postContent "\"2019-04-01T16:00:00+05:00\""
            runWith app 
            |> req HttpMethod.Post "/IProtocol/datetimeOffset" (Some input)
            |> fun result -> equal "\"2019-04-01T16:00:00+05:00\"" result

        testCase "Maybe<DateTimeOffset> roundtrip" <| fun () -> 
            let input = postContent "{\"Just\":\"2019-04-01T16:00:00+05:00\"}"
            runWith app 
            |> req HttpMethod.Post "/IProtocol/maybeDatetimeOffset" (Some input)
            |> fun result -> equal "{\"Just\":\"2019-04-01T16:00:00+05:00\"}" result

        testCase "Sending some option as input works" <| fun () ->
            let someInput = postContent "5" // toJson (Some 5) => "5"
            let testApp = runWith app
            testApp
            |> req HttpMethod.Post "/IProtocol/echoOption" (Some someInput)
            |> fun result -> equal "10" result

        testCase "Sending none option as input works" <| fun () ->
            // the string "null" represents None
            // it's what fable sends from browser
            let noneInput = postContent "null" // toJson None => "null"
            let testApp = runWith app
            
            testApp
            |> req HttpMethod.Post "/IProtocol/echoOption" (Some noneInput)
            |> fun result -> equal "0" result

        
        testCase "Sending DateTime as input works" <| fun () -> 
            let someInput = postContent "\"2017-05-12T14:20:00.000Z\""
            let testApp = runWith app
            testApp
            |> req HttpMethod.Post "/IProtocol/echoMonth" (Some someInput)
            |> equal "5"

        testCase "Sending Result<int, string> roundtrip works with Ok" <| fun _ ->
            let input = postContent (toJson (Ok 15))
            runWith app
            |> req HttpMethod.Post "/IProtocol/echoResult" (Some input)
            |> ofJson<Result<int, string>>
            |> function 
                | Ok 15 -> pass()
                | otherwise -> fail()

        testCase "Sending long in single case du roundtrip" <| fun _ ->
            let input = postContent (toJson (SingleLongCase 20L))
            runWith app
            |> req HttpMethod.Post "/IProtocol/echoSingleDULong" (Some input)
            |> ofJson<SingleLongCase> 
            |> function 
                | SingleLongCase 20L -> pass()
                | otherwise -> fail()
        
        testCase "Thrown error is catched and returned" <| fun _ -> 
            let input = postContent "\"\""
            runWith app
            |> req HttpMethod.Post "/IProtocol/throwError" (Some input)
            |> ofJson<CustomErrorResult<string>>
            |> equal { error = "I am thrown from adapter function";
                       handled = true;
                       ignored = false }

        testCase "Sending Result<int, string> roundtrip works with Error" <| fun _ ->
            let input = postContent (toJson (Error "hello"))
            runWith app
            |> req HttpMethod.Post "/IProtocol/echoResult" (Some input)
            |> ofJson<Result<int, string>>
            |> function 
                | Error "hello" -> pass()
                | otherwise -> fail()
            
        testCase "Sending BigInteger roundtrip works" <| fun _ ->
            let input = postContent (toJson [1I .. 5I])
            runWith app
            |> req HttpMethod.Post "/IProtocol/echoBigInteger" (Some input)
            |> ofJson<bigint>
            |> function 
                | sum when sum = 15I -> pass()
                | otherwise -> fail()

        testCase "Sending Map<string, int> roundtrips works" <| fun _ ->
            let inputMap = ["one",1; "two",2] |> Map.ofList
            let input = postContent (toJson inputMap)
            runWith app
            |> req HttpMethod.Post "/IProtocol/echoMap" (Some input)
            |> ofJson<Map<string, int>>
            |> Map.toList
            |> function 
                | ["one",1; "two",2] -> pass()
                | otherwise -> fail() 

        testCase "Sending and recieving strings works" <| fun () -> 
            let someInput = postContent "\"my-string\""
            let testApp = runWith app
            testApp
            |> req HttpMethod.Post "/IProtocol/echoString" (Some someInput)
            |> equal "\"my-string\""     
            
        testCase "Recieving int option to None output works" <| fun () -> 
            let someInput = postContent "\"\""
            let testApp = runWith app
            testApp
            |> req HttpMethod.Post "/IProtocol/optionOutput" (Some someInput)
            |> equal "null" 
            
        testCase "Recieving int option to Some output works" <| fun () -> 
            let someInput = postContent "\"non-empty\""
            let testApp = runWith app
            testApp
            |> req HttpMethod.Post "/IProtocol/optionOutput" (Some someInput)
            |> equal "5"
            
        testCase "Sending generic union case Nothing as input works" <| fun () ->
            let someInput = postContent "\"Nothing\""
            let testApp = runWith app
            testApp
            |> req HttpMethod.Post "/IProtocol/genericUnionInput" (Some someInput)
            |> equal "0"      
            
        testCase "Sending generic union case Just as input works" <| fun () -> 
            let someInput = postContent "{\"Just\":5}"
            let testApp = runWith app
            testApp
            |> req HttpMethod.Post "/IProtocol/genericUnionInput" (Some someInput)
            |> equal "5" 
            
        
        testCase "Recieving generic union case Just 5 as output works" <| fun () -> 
            let someInput = postContent "true"
            let testApp = runWith app
            testApp
            |> req HttpMethod.Post "/IProtocol/genericUnionOutput" (Some someInput)
            |> equal "{\"Just\":5}"

        
        testCase "Recieving generic union case Nothing as output works" <| fun () ->
            let someInput = postContent "false"
            let testApp = runWith app
            testApp
            |> req HttpMethod.Post "/IProtocol/genericUnionOutput" (Some someInput)
            |> equal "\"Nothing\""

        testCase "Recieving and sending simple union works" <| fun () -> 
            let someInput = postContent "\"A\""
            let testApp = runWith app
            testApp
            |> req HttpMethod.Post "/IProtocol/simpleUnionInputOutput" (Some someInput)
            |> equal "\"B\""
            
        testCase "Recieving and sending records works" <| fun () -> 
            // In Fable, toJson { Prop1 = ""; Prop2 = 5; Prop3 = None }
            // becomes
            let recordInput = postContent "{\"Prop1\":\"\",\"Prop2\":5,\"Prop3\":null}"
            let testApp = runWith app
            testApp
            |> req HttpMethod.Post "/IProtocol/recordEcho" (Some recordInput)
            |> equal "{\"Prop1\":\"\",\"Prop2\":15,\"Prop3\":null}" 

        testCase "Sending list of ints works" <| fun () -> 
            let someInput = postContent "[1,2,3,4,5,6,7,8,9,10]"
            let testApp = runWith app
            testApp
            |> req HttpMethod.Post "/IProtocol/listIntegers" (Some someInput)
            |> equal "55" 

        testCase "Inoking function of unit works" <| fun () -> 
            // server will ignore the input
            let someInput = postContent ""
            let testApp = runWith app
            testApp
            |> req HttpMethod.Post "/IProtocol/unitToInts" (Some someInput)
            |> equal "55" 

        testCase "Invoking list of records works" <| fun () ->
            let someInput = postContent "[{\"Prop1\":\"\",\"Prop2\":15,\"Prop3\":null}, {\"Prop1\":\"\",\"Prop2\":10,\"Prop3\":null}]"
            let testApp = runWith app
            testApp
            |> req HttpMethod.Post "/IProtocol/recordListToInt" (Some someInput)
            |> equal "25" 

        testCase "Invoking a list of float works" <| fun () -> 
            let someInput = postContent "[1.20, 1.40, 1.60]"
            let testApp = runWith app
            testApp
            |> req HttpMethod.Post "/IProtocol/floatList" (Some someInput)
            |> equal "4.2"

        testCase "Invoking with two arguments works" <| fun () -> 
            let someInput = postRaw "[13, 17]"
            let testApp = runWith app
            testApp
            |> req HttpMethod.Post "/IProtocol/multipleSum" (Some someInput)
            |> equal "30"

        testCase "Invoking with lots of arguments works" <| fun () -> 
            let someInput = postRaw "[\"Test\", 17, 5.0]"
            let testApp = runWith app
            testApp
            |> req HttpMethod.Post "/IProtocol/lotsOfArgs" (Some someInput)
            |> equal "\"string: Test; int: 17; float: 5.000000\""

            
    ]
