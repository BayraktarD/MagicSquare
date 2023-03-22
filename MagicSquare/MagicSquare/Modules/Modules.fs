module File1 




module MagicSquare = 

    //Pătrate magice de ordin impar (I)
    //https://www.1728.org/magicsq1.htm
    let rec createOddMagicSquare (order:int)=
        let square = Array2D.create order order 0
        let mutable row = 0         //primul rând
        let mutable col = order / 2 //căsuța centrală 
        for num in 1 .. order * order do 
            square.[row, col] <- num //inserare numar 
            let nextRow = (row - 1 + order) % order
            let nextCol = (col + 1) % order
            if square.[nextRow, nextCol] <> 0 then
                row <- (row + 1) % order
                else
                    row <- nextRow
                    col <- nextCol
        square


    //https://www.1728.org/magicsq2.htm
    let createDoublyEvenMagicSquare order =
        let square = Array2D.create order order 0
        let mutable row = 0
        let mutable col = 0

        let mutable cornerMiniSquaresOrder = order / 4
        let mutable centerMiniSquareOrder = order / 2

        let mutable num = 1
        let mutable numEnd = order*order

        //completeaza cadranul din stanga sus si dreapta sus
        while row < order / 4 do 
            while col < order / 4 do
                square[row,col] <- num
                num <- num + 1
                numEnd <- numEnd - 1
                col <- col + 1
            while col < order - (order/4) do
                square[row,col]<-numEnd
                numEnd <- numEnd - 1
                num <- num + 1
                col<-col+1
            while col < order do
                square[row,col]<-num
                num<-num+1
                numEnd <- numEnd - 1
                col<-col+1
            col <- 0
            row <- row + 1

        //completeaza cadranul central si 
        while (row >= (order / 4) && row < (order - (order / 4))) do
            while col < order / 4 do
                square[row,col] <- numEnd
                numEnd <- numEnd - 1
                num <- num + 1
                col <- col + 1
            while col < order - (order/4) do
                square[row,col]<-num
                num <- num + 1
                numEnd <- numEnd - 1
                col<-col+1
            while col < order do
                square[row,col]<-numEnd
                numEnd <- numEnd - 1
                num <- num + 1
                col <- col+1
            col <- 0
            row <- row + 1

        while row >= (order - (order / 4)) && row < order do
            while col < order / 4 do
                square[row,col] <- num
                num <- num + 1
                numEnd <- numEnd - 1
                col <- col + 1
            while col < order - (order/4) do
                square[row,col]<-numEnd
                numEnd <- numEnd - 1
                num <- num + 1
                col<-col+1
            while col < order do
                square[row,col]<-num
                num<-num+1
                numEnd <- numEnd - 1
                col<-col+1
            col <- 0
            row <- row + 1

        square


    //https://www.1728.org/magicsq3.htm
    //https://math.uni.lu/eml/assets/reports/Magic_Squares.pdf
    let rec createOddMiniMagicSquares (order:int) (firstNum:int) (lastNum:int)=
        let square = Array2D.create order order 0
        let mutable row = 0         //primul rând
        let mutable col = order / 2 //căsuța centrală 
        for num in firstNum .. lastNum do 
            square.[row, col] <- num //inserare numar 
            let nextRow = (row - 1 + order) % order
            let nextCol = (col + 1) % order
            if square.[nextRow, nextCol] <> 0 then
                row <- (row + 1) % order
                else
                    row <- nextRow
                    col <- nextCol
        square

    let singlyEvenMagicSquare order = 
        let miniSquaresOrder = order/2;
        let k = (miniSquaresOrder-1)/2
        let centralRowMiniSquare = miniSquaresOrder / 2

        let mutable row = 0
        let mutable col = 0

        //SQUARE A
        let mutable firstNum = 1
        let mutable lastNum = order*order/4
        let squareA = createOddMiniMagicSquares miniSquaresOrder firstNum lastNum

        //SQUARE B
        firstNum <- 1 + order * order / 4
        lastNum <- order * order / 2
        let squareB = createOddMiniMagicSquares miniSquaresOrder firstNum lastNum
      
        //SQUARE C
        firstNum <- 1 + order*order/2
        lastNum <- 3 * order * order / 4 
        let squareC = createOddMiniMagicSquares miniSquaresOrder firstNum lastNum

        //SQUARE D
        firstNum <- 1 + 3 * order * order / 4
        lastNum <- order * order 
        let squareD = createOddMiniMagicSquares miniSquaresOrder firstNum lastNum

        let mutable valueA = 0
        let mutable valueD = 0
        for i = 0 to miniSquaresOrder - 1 do
            for j = 0 to k-1 do
                if i <> centralRowMiniSquare then
                    valueA <- squareA.[i,j]
                    valueD <- squareD.[i,j]
                    squareA.[i,j] <- valueD
                    squareD.[i,j] <- valueA
                    else 
                        valueA <- squareA.[i,j+1]
                        valueD <- squareD.[i,j+1]
                        squareA.[i,j+1] <- valueD
                        squareD.[i,j+1] <- valueA


         
        let mutable valueB = 0
        let mutable valueC = 0

        for i = 0 to miniSquaresOrder - 1 do
            for j = miniSquaresOrder - k + 1  to miniSquaresOrder - 1 do
                valueB <- squareB.[i,j]
                valueC <- squareC.[i,j]
                squareB.[i,j] <- valueC
                squareC.[i,j] <- valueB

        let squareABCD = Array2D.zeroCreate order order
        for i = 0 to order - 1 do
            for j = 0 to order - 1 do
                if i < miniSquaresOrder then
                    if j < miniSquaresOrder then
                        squareABCD.[i, j] <- squareA.[i, j] 
                        else
                        squareABCD.[i, j] <- squareC.[i, j-miniSquaresOrder]
                else
                    if j < miniSquaresOrder then
                        squareABCD.[i, j] <- squareD.[i - miniSquaresOrder, j] 
                        else
                            squareABCD.[i, j] <- squareB.[i- miniSquaresOrder, j - miniSquaresOrder]
                        
        squareABCD

        
    
    
   
 

