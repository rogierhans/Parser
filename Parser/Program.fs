open FParsec
type Packet = Literal of (int * int) * int64 | Operator of (int * int) *  Packet list | Error of string
let Hex2Bits (a:string) =
    match a with
    |"0"  -> "0000"
    |"1"  -> "0001"
    |"2"  -> "0010"
    |"3"  -> "0011"
    |"4"  -> "0100"
    |"5"  -> "0101"
    |"6"  -> "0110"
    |"7"  -> "0111"
    |"8"  -> "1000"
    |"9"  -> "1001"
    |"A"  -> "1010"
    |"B"  -> "1011"
    |"C"  -> "1100"
    |"D"  -> "1101"
    |"E"  -> "1110"
    |"F"  -> "1111"
    | _    -> ":^)"

let teststring1 = "D2FE28"
let teststring2 = "38006F45291200"
let teststring3 = "EE00D40C823060"
let teststring4 = "8A004A801A8002F478"
let teststring5 = "620080001611562C8802118E34"
let teststring6 = "C0015000016115A2E0802F182340"
let teststring7 = "A0016C880162017C3686B18A3D4780"
let teststring8 = "820D4A801EE00720190CA005201682A00498014C04BBB01186C040A200EC66006900C44802BA280104021B30070A4016980044C800B84B5F13BFF007081800FE97FDF830401BF4A6E239A009CCE22E53DC9429C170013A8C01E87D102399803F1120B4632004261045183F303E4017DE002F3292CB04DE86E6E7E54100366A5490698023400ABCC59E262CFD31DDD1E8C0228D938872A472E471FC80082950220096E55EF0012882529182D180293139E3AC9A00A080391563B4121007223C4A8B3279B2AA80450DE4B72A9248864EAB1802940095CDE0FA4DAA5E76C4E30EBE18021401B88002170BA0A43000043E27462829318F83B00593225F10267FAEDD2E56B0323005E55EE6830C013B00464592458E52D1DF3F97720110258DAC0161007A084228B0200DC568FB14D40129F33968891005FBC00E7CAEDD25B12E692A7409003B392EA3497716ED2CFF39FC42B8E593CC015B00525754B7DFA67699296DD018802839E35956397449D66997F2013C3803760004262C4288B40008747E8E114672564E5002256F6CC3D7726006125A6593A671A48043DC00A4A6A5B9EAC1F352DCF560A9385BEED29A8311802B37BE635F54F004A5C1A5C1C40279FDD7B7BC4126ED8A4A368994B530833D7A439AA1E9009D4200C4178FF0880010E8431F62C880370F63E44B9D1E200ADAC01091029FC7CB26BD25710052384097004677679159C02D9C9465C7B92CFACD91227F7CD678D12C2A402C24BF37E9DE15A36E8026200F4668AF170401A8BD05A242009692BFC708A4BDCFCC8A4AC3931EAEBB3D314C35900477A0094F36CF354EE0CCC01B985A932D993D87E2017CE5AB6A84C96C265FA750BA4E6A52521C300467033401595D8BCC2818029C00AA4A4FBE6F8CB31CAE7D1CDDAE2E9006FD600AC9ED666A6293FAFF699FC168001FE9DC5BE3B2A6B3EED060"

let bits1:string = String.concat "" ( Seq.map ( string>>Hex2Bits ) teststring1)
let bits2:string = String.concat "" ( Seq.map ( string>>Hex2Bits ) teststring2)
let bits3:string = String.concat "" ( Seq.map ( string>>Hex2Bits ) teststring3)
let bits4:string = String.concat "" ( Seq.map ( string>>Hex2Bits ) teststring4)
let bits5:string = String.concat "" ( Seq.map ( string>>Hex2Bits ) teststring5)
let bits6:string = String.concat "" ( Seq.map ( string>>Hex2Bits ) teststring6)
let bits7:string = String.concat "" ( Seq.map ( string>>Hex2Bits ) teststring7)
let bits8:string = String.concat "" ( Seq.map ( string>>Hex2Bits ) teststring8)

let one = pstring "1" >>% "1"
let zero = pstring "0" >>% "0"
let bit = one <|> zero

let rec repeatParser p number = 
    match number with 
    | 0 -> preturn []
    | _ -> pipe2 p (repeatParser p (number-1)) (fun a b -> (a::b))


    

let rec Flat list =
    match list with
    | [] ->""
    | (x::xs) -> x + Flat xs


let repeatBits number = repeatParser bit number |>> Flat
let bitsToInt x = System.Convert.ToInt32(x,2)
let bitsToLong x = System.Convert.ToInt64(x,2)
let block = one >>. repeatBits 4
let lastBlock = zero >>.  repeatBits 4
let version = repeatBits 3 |>>  bitsToInt
let packtetID = repeatBits 3 |>>  bitsToInt 
let literalID = pstring "100" >>% 4
let blockBitsParser =pipe2 (many block) lastBlock (fun x y -> ( bitsToLong) (Flat x + y) )
let lengthLabel length =  repeatBits length |>> (bitsToInt)
let subsrting = lengthLabel 15 >>=  repeatBits 


let rec packetParser = literal <|>operator0 <|> operator1
and  strToPackets  str =
    match run (many packetParser) str with
    | Success(result, _, _)   ->  result
    | Failure(errorMsg, _, _) -> [Error errorMsg] 
and literal = version  .>>.?  literalID .>>.?   blockBitsParser  |>> Literal
and operator0 = version  .>>.? packtetID .>>? zero .>>. subsrting |>> fun (x,d) ->  Operator (x,strToPackets d)
and operator1 =   version  .>>.? packtetID  .>>? one  .>>. multiplePacket |>> Operator
and multiplePacket = lengthLabel 11 >>=( fun length ->  repeatParser packetParser length )



let min a b = if a < b then a else b
let max a b = if a < b then b else a
let rec eval (pak:Packet) =
    match pak with
    | Literal ((_,_),x) -> x
    | Operator ((_,p),list) ->  applyOperator p list
    | Error (_) -> 0L
and applyOperator p list = 
    match p with
    | 0 -> List.sum (List.map eval list)
    | 1 -> List.fold (fun a b -> a * b ) 1 (List.map eval list)
    | 2 -> List.fold min System.Int64.MaxValue (List.map eval list)
    | 3 -> List.fold max System.Int64.MinValue (List.map eval list)
    | 5 -> if (List.map eval list)[0]> (List.map eval list)[1] then 1 else 0
    | 6 -> if (List.map eval list)[0]< (List.map eval list)[1] then 1 else 0
    | 7 ->if (List.map eval list)[0] = (List.map eval list)[1] then 1 else 0
    | _ ->  System.Int64.MaxValue
type Floater = {Name : string; Age:float} 

let strToPacket  str =
    match run packetParser str with
    | Success(result, _, _)   ->  result
    | Failure(errorMsg, _, _) -> Error errorMsg


let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg


printfn "Result: %A" ( eval (strToPacket  bits1))
printfn "Result: %A" ( eval (strToPacket  bits2))
printfn "Result: %A" ( eval (strToPacket  bits3))
printfn "Result: %A" ( eval (strToPacket  bits4))
printfn "Result: %A" ( eval (strToPacket  bits5))
printfn "Result: %A" ( eval (strToPacket  bits6))
printfn "Result: %A" ( eval (strToPacket  bits7))
printfn "Result: %A" ( eval (strToPacket  bits8))