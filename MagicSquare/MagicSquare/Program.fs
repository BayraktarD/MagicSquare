open System
open File1


[<EntryPoint>]
let main argv =
        printfn "Enter the order of the magic square:"
        let input = Console.ReadLine()
        match Int32.TryParse(input) with
        | true, order when order >= 3 ->
            let magicSquare = 
                if order % 2 = 1 then
                    File1.MagicSquare.createOddMagicSquare order
                else
                    if order % 2 = 0 && order % 4 <> 0 then
                        File1.MagicSquare.singlyEvenMagicSquare order
                        else
                        File1.MagicSquare.createDoublyEvenMagicSquare order

            printfn "Magic square of order %d:" order
            for row in 0 .. order - 1 do
                for col in 0 .. order - 1 do
                    printf "%3d " magicSquare.[row, col] 
            
                printfn "            Row %d sum: %d" row (magicSquare.[row, 0..order-1] |> Array.sum)
                printfn ""
        
            for col in 0 .. order - 1 do
                printfn "Column %d sum: %d" col (magicSquare.[0..order-1, col] |> Array.sum)

            0
        | true, _ ->
            printfn "Invalid order. Please enter an odd integer greater than or equal to 3 or a multiple of 4."
            1
        | _ ->
            printfn "Invalid input. Please enter an integer."
            1



