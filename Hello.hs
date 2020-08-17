module Main(main) where

import Data.List
import Data.Char


-- nome e kcal/g, tranne che per i tipi 
cibi = [ ("pane" , \a -> a / 100.0 * 270.0 ),
         ("tarallini",\a -> a * 19.0 ), -- al pezzo
         ("yogurt", \a -> a * 1.0 ),
         ("banana+buccia", \a -> ( a - 60.0 ) * 65.0 / 100.0),
         ("mozzarella", \a -> a * 300.0 / 125.0 ),
         ("cetriolo", \a -> a * 12.0 / 100.0),
         ("cucchiaio_olio", \a -> a * 80.0 ), -- al cucchiaio
         ("pomodoro", \a -> a * 18.0 / 100.0),
         ("mais", \y -> y *80.0 / 100.0 ),
         ("cipolla", \y -> y *40.0/100.0),
         ("pollo_forno", \y -> y * 250.0 / 100.0 * 3.0 / 4.0 ), -- considero le ossa come 1/4 del peso
         ("senape", \y -> y * 66.0 /100.0 ),
         ("krapfen", \y -> y * 380.0 / 100.0 ),
         ("germogli_soia", \y -> y * 17.0 / 100.0 ),
         ("dolcettini", \y -> y* 80.0), -- al pezzo
         ("tacchino", \y -> y),
         ("scamorzine", \y -> y* 290.0/100.0 )
       ]

cibo a = [ x | (name,x) <- cibi, name `isInfixOf` a ]

calorie x  | null l = (\y -> y, False, x)
           | otherwise = (head l, True, x)
           where l = cibo x


data Split = Split {splitPos::Int, splitTxt::String} deriving (Show)


lastPos p  | null p = 0
           | otherwise = splitPos ( last p )

lastTxt p  | null p = ""
           | otherwise = splitTxt ( last p )

newPos p = lastPos p +  ( length (lastTxt p) )

initE p | null p = []
        | otherwise = init p

splitter p (x:xs) spaces
                  | x == ' ' && ( not (null p) ) && splitTxt (last p) == "" = splitter p xs (spaces + 1)
                  | x == ' ' = splitter ( p ++ [ Split (newPos p) "" ] ) xs (spaces + 1 )
                  | otherwise = splitter ( initE p ++ [ Split (lastPos p + spaces) (lastTxt p ++ [x] ) ] ) xs 0
splitter p [] a = p


data FoodErr = FoodErr { fErrors::String, fOut::[String], fInp::[Split], fSum::Double, fEnd::Bool }


fConcat (FoodErr e o i s end) f = let (FoodErr e_ o_ i_ s_ end_) = f (last o) i
                           in FoodErr (e ++ e_) (o ++ o_) i_ (s + s_) (end || end_)

calCheck (cal, succ, food ) x pos | succ = ( cal x, "")
                           | otherwise = ( cal x, "Error: undefined food '"++food++"' before col "++ (show pos) ++ "\n")

convert lastp (Split pos txt :xs)
                 | any (\y -> not ( ('.' == y) || ( isDigit y ) ) ) txt = FoodErr "" [txt] xs 0 False
                 | otherwise = let (cal, etxt) = (calCheck (calorie lastp) (read txt::Double ) pos)
                               in  FoodErr etxt [show cal] xs cal False
convert p [] = FoodErr "" [] [] 0 True

convertM foodErr |  fEnd foodErr = (fOut foodErr, fErrors foodErr, fSum foodErr)
                 |  otherwise = convertM (foodErr `fConcat` convert)

convertL line = convertM (FoodErr { fErrors = "" , fOut = [] , fInp = (splitter [] line 0), fSum = 0.0, fEnd = False } )

main = do
   line <- getLine 
   if null line then return ()
   else do
       putStrLn ( show $ convertL line )
       main

