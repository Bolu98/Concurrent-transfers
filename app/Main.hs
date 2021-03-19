module Main where

import Control.Concurrent
import System.Random
import Control.Parallel
import Control.Parallel.Strategies

-- | Aliases are given to String, Int and Float datatypes to allow for more readable definitions
type Name = String
type AccountNumber = Int
type AccountBalance = Float

{- |
Each customer has a name of type Name (String), account number of type AccountNumber(Int), and an
account number of type AccountBalance(Float)
-}
data Customer = Customer {customerName :: Name, customerAcctNum :: AccountNumber, customerBal :: AccountBalance } deriving(Show, Eq)

-- | This functions generates an array of 10 customers using record syntax
generateCustomers :: [Customer]
generateCustomers =
    let 
        c0 = Customer{customerName = "John Smith", customerAcctNum = 12345, customerBal = 1000 }
        c1 = Customer{customerName = "Jane Doe", customerAcctNum = 67891, customerBal = 1000 }
        c2 = Customer{customerName = "Robert Downie", customerAcctNum = 11121, customerBal = 1000 }
        c3 = Customer{customerName = "Rachel Fern", customerAcctNum = 14151, customerBal = 1000 }
        c4 = Customer{customerName = "Tom Jerry", customerAcctNum = 17181, customerBal = 1000 }
        c5 = Customer{customerName = "Lily Rose", customerAcctNum = 20212, customerBal = 1000 }
        c6 = Customer{customerName = "Jeremy Trout", customerAcctNum = 23242, customerBal = 1000 }
        c7 = Customer{customerName = "Maya May", customerAcctNum = 26272, customerBal = 1000 }
        c8 = Customer{customerName = "Tom Bush", customerAcctNum = 29303, customerBal = 1000 }
        c9 = Customer{customerName = "Raven Simpson", customerAcctNum = 32333, customerBal = 1000 }
    in [c0, c1, c2, c3, c4, c5, c6, c7, c8, c9]

-- | A custom print function is defined to display each customer's details at the start of program execution 
printCustomer :: Customer -> String
printCustomer (Customer {customerName = name, customerAcctNum = num, customerBal = bal}) = 
    "Customer " ++ name ++ " with account number " ++ show num ++ " has a balance of £" ++ show bal

-- | A custom printing functions for arrays of strings using the mapM_ monad
printCustomerArray :: [String] -> IO()
printCustomerArray = mapM_ putStrLn

-- | This function outputs the account balance of a customer provided as input
obtainBalance :: Customer -> AccountBalance
obtainBalance (Customer {customerName = name, customerAcctNum = num, customerBal = bal}) = bal

-- | A custom MVar function that returns an array of MVars which are used to store the account balances
generateEmptyMVar :: IO [MVar AccountBalance]
generateEmptyMVar = do
    box0 <- newEmptyMVar
    box1 <- newEmptyMVar
    box2 <- newEmptyMVar
    box3 <- newEmptyMVar
    box4 <- newEmptyMVar
    box5 <- newEmptyMVar
    box6 <- newEmptyMVar
    box7 <- newEmptyMVar
    box8 <- newEmptyMVar
    box9 <- newEmptyMVar
    return [box0, box1, box2, box3, box4, box5, box6, box7, box8, box9]

{-|
A custom MVar function at takes an MVar (account balance) and an account balance as 
input and stores the customer balance in the MVar box
-}
putMVarTuple :: (MVar AccountBalance, AccountBalance) -> IO ()
putMVarTuple (box, customer) = do 
    putMVar box customer


-- | A function that generates a random number that is not equal to the number given as input
generateRandomIndex :: Int -> IO Int
generateRandomIndex n = do
    x <- randomRIO(0,9) :: IO Int
    if x /= n then return x else generateRandomIndex n

{- |
The function that performs the money transfer action. It takes to MVars
and removes the values inside. If these values are between 10 and 50, then
money is transfered from the first customer to the second customer and their
MVars are updated accordingly. Otherwise, their values stay the same
-}
transferMoney :: MVar AccountBalance -> MVar AccountBalance -> IO ()
transferMoney box1 box2 = do
    bal1 <- takeMVar box1
    bal2 <- takeMVar box2
    if bal1 > 10 
        then do
            amount <- randomRIO (10, (min 50 bal1)) :: IO Float
            putMVar box1 $ bal1 - amount
            putMVar box2 $ bal2 + amount
        else do 
            putMVar box1 bal1
            putMVar box2 bal2

-- | A function to perform a looping action so that 100 transfers can be made
loop :: Int -> (IO ThreadId) -> IO ()
loop 0 _ = return ()
loop n f = do
    f
    loop (n-1) f

-- | Another custom print function to allow for printing each customer's final balance
printFinalBalance :: Customer -> AccountBalance -> String
printFinalBalance (Customer {customerName = name, customerAcctNum = num, customerBal = bal}) box = 
    "Customer " ++ name ++ " with account number " ++ show num ++ " has a balance of £" ++ show box


{- |
The main function generated 10 customers, spawns 10 threads, performs 100 transfers (10 for each customer) and
displays each customer's final balance as well as the total amount of money available.
-} 
main :: IO ()
main = do
    putStrLn "--- Generating customers ---"
    let customerArr = generateCustomers 
    printCustomerArray $ (parMap rdeepseq) printCustomer customerArr
    let customerBalanceArr = (parMap rdeepseq) obtainBalance customerArr
    putStrLn "--- Spawn threads ---"
    boxArr <- generateEmptyMVar
    let tmp = zip boxArr customerBalanceArr
    mapM_ putMVarTuple tmp
    putStrLn "--- Commence transfers ---"
    loop 10 (do
        t0 <- randomRIO(10, 1000) :: IO Int
        threadDelay t0
        idx0 <- generateRandomIndex 0
        forkIO (transferMoney (boxArr !! 0) (boxArr !! idx0))
        t1 <- randomRIO(10, 1000) :: IO Int
        threadDelay t1
        idx1 <- generateRandomIndex 1
        forkIO (transferMoney (boxArr !! 1) (boxArr !! idx1))
        t2 <- randomRIO(10, 1000) :: IO Int
        threadDelay t2
        idx2 <- generateRandomIndex 2
        forkIO (transferMoney (boxArr !! 2) (boxArr !! idx2))
        t3 <- randomRIO(10, 1000) :: IO Int
        threadDelay t3
        idx3 <- generateRandomIndex 3
        forkIO (transferMoney (boxArr !! 3) (boxArr !! idx3))
        t4 <- randomRIO(10, 1000) :: IO Int
        threadDelay t4
        idx4 <- generateRandomIndex 4
        forkIO (transferMoney (boxArr !! 4) (boxArr !! idx4))
        t5 <- randomRIO(10, 1000) :: IO Int
        threadDelay t5
        idx5 <- generateRandomIndex 5
        forkIO (transferMoney (boxArr !! 5) (boxArr !! idx5))
        t6 <- randomRIO(10, 1000) :: IO Int
        threadDelay t6
        idx6 <- generateRandomIndex 6
        forkIO (transferMoney (boxArr !! 6) (boxArr !! idx6))
        t7 <- randomRIO(10, 1000) :: IO Int
        threadDelay t7
        idx7 <- generateRandomIndex 7
        forkIO (transferMoney (boxArr !! 7) (boxArr !! idx7))
        t8 <- randomRIO(10, 1000) :: IO Int
        threadDelay t8
        idx8 <- generateRandomIndex 8
        forkIO (transferMoney (boxArr !! 8) (boxArr !! idx8))
        t9 <- randomRIO(10, 1000) :: IO Int
        threadDelay t9
        idx9 <- generateRandomIndex 9
        forkIO (transferMoney (boxArr !! 9) (boxArr !! idx9))
    --     let f0 = forkIO (transferMoney box0 (boxArr !! idx0))
    --         f1 = forkIO (transferMoney box1 (boxArr !! idx1))
    --         f2 = forkIO (transferMoney box2 (boxArr !! idx2))
    --         f3 = forkIO (transferMoney box3 (boxArr !! idx3))  
    --         f4 = forkIO (transferMoney box4 (boxArr !! idx4))
    --         f5 = forkIO (transferMoney box5 (boxArr !! idx5))
    --         f6 = forkIO (transferMoney box6 (boxArr !! idx6))
    --         f7 = forkIO (transferMoney box7 (boxArr !! idx7))
    --         f8 = forkIO (transferMoney box8 (boxArr !! idx8))
    --         f9 = forkIO (transferMoney box9 (boxArr !! idx9))
    --         p1 = par (par (f0) (f1)) (par (f2) (f3))
    --         p2 = par (par (f4) (f5)) (par (f6) (f7))
    --         p3 = par p1 p2
    --         p4 = par (f8) (f9)
    --     par p3 p4 
        )
    putStrLn "--- Display final balances ---"
    endBal0 <- takeMVar (boxArr !! 0)
    putStrLn $ printFinalBalance (customerArr !! 0) endBal0
    endBal1 <- takeMVar (boxArr !! 1)
    putStrLn $ printFinalBalance (customerArr !! 1) endBal1
    endBal2 <- takeMVar (boxArr !! 2)
    putStrLn $ printFinalBalance (customerArr !! 2) endBal2
    endBal3 <- takeMVar (boxArr !! 3)
    putStrLn $ printFinalBalance (customerArr !! 3) endBal3
    endBal4 <- takeMVar (boxArr !! 4)
    putStrLn $ printFinalBalance (customerArr !! 4) endBal4
    endBal5 <- takeMVar (boxArr !! 5)
    putStrLn $ printFinalBalance (customerArr !! 5) endBal5
    endBal6 <- takeMVar (boxArr !! 6)
    putStrLn $ printFinalBalance (customerArr !! 6) endBal6
    endBal7 <- takeMVar (boxArr !! 7)
    putStrLn $ printFinalBalance (customerArr !! 7) endBal7
    endBal8 <- takeMVar (boxArr !! 8)
    putStrLn $ printFinalBalance (customerArr !! 8) endBal8
    endBal9 <- takeMVar (boxArr !! 9)
    putStrLn $ printFinalBalance (customerArr !! 9) endBal9
    putStrLn $ "Cumulative end sum : " ++ show (endBal0+endBal1+endBal2+endBal3+endBal4+endBal5+endBal6+endBal7+endBal8+endBal9)
    putStrLn "--- Finished ---"