open System;

//Finds the amount of characters in a Text
let rec CharacterAmount (number : int, WordV : string) =
    match WordV[number..number] with
        | "" -> number
        | _ -> CharacterAmount (number+1, WordV)  

//Finds the amount of words in a text
let rec WordAmount (number : int, wordsV : string array) =
    if (number < wordsV.Length) then
        WordAmount(number+1, wordsV)
    else
        number


//Finds the amount of Sentences in a text
let rec sentenceAmount (number : int, sentenceV : string array) =
    if (number < sentenceV.Length) then
        sentenceAmount(number+1, sentenceV)
    else
        number-1

//Finds the accurence of Words
let rec accurenceOfWords (reacurring : Map<String, int>, number : int, wordsV : string array) =
    if number < wordsV.Length then
        // Removes puncuation and makes it lowercase
        let currentWord = wordsV[number].ToLower().Trim([|',';'.';'!';'?'|])

        if reacurring.ContainsKey(currentWord) then
            let reacurringChange = reacurring.Add(currentWord, reacurring.[currentWord] + 1)
            accurenceOfWords(reacurringChange, number + 1, wordsV)
        else
            let reacurringChange = reacurring.Add(currentWord, 1)
            accurenceOfWords(reacurringChange, number + 1, wordsV)
    else
        reacurring


// Converts the character using a cypher shift
let convert(Character : char) =
    let Alphabet = ['a'..'z']

    if List.contains Character Alphabet then
        let i = Alphabet |> (List.findIndex ((=) Character)) 
        let ReturnCharacter = Alphabet.[(i + 5) % 26]
        ReturnCharacter
    else
        Character
    
    
let CeaserCypher(CC : string) = 
    CC.ToLower()
    |> Seq.toList
    |> Seq.map(convert)
 
Console.WriteLine("Write String that you would like tested: ")
let Word = Console.ReadLine()
    
let ReacurringWords = Map.empty
let words = Word.Split[|' '|]      
let Sentences = Word.Split [|'.'; '?'; '!'|]

printfn "\n"
printfn "Character Amount: %d" (CharacterAmount(0, Word));
printfn "Word Amount: %d" (WordAmount(0, words));
printfn "Sentence Amount: %d" (sentenceAmount(0, Sentences));

printfn "\n-------------------------------------"
printfn "Occurrence of words:"
accurenceOfWords(ReacurringWords, 0, words) |> Map.iter (fun k v -> printfn($"{k}: {v}"));
printfn "-------------------------------------\n"

printfn "CeaserCypher: %A" (CeaserCypher(Word) |> String.Concat);

