import Data.List

l1="        ***** **            ***** **    "
l2="     ******  ****        ******  ****   "
l3="    **   *  *  ***      **   *  *  ***  "
l4="   *    *  *    ***    *    *  *    *** "
l5="       *  *      **        *  *      ** "
l6="      ** **      **       ** **      ** "
l7="      ** **      **       ** **      ** "
l8="    **** **      *      **** **      *  "
l9="   * *** **     *      * *** **     *   "
l10="      ** *******          ** *******    "
l11="      ** ******           ** ******     "
l12="      ** **               ** **         "
l13="      ** **               ** **         "
l14="      ** **               ** **         "
l15=" **   ** **          **   ** **         "
l16="***   *  *          ***   *  *          "
l17=" ***    *            ***    *           "
l18="  ******              ******            "
l19="    ***                 ***             "
 
logo = [l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11,l12,l13,l14,l15,l16,l17,l18,l19]
--Matrix : [ [1,2,3],[4,5,6],[7,8,9] ].
type Matrix = [[Integer]] 

--Ex1. Write a function parsem which takes a string and parses it to a Matrix. 
--In the string, the columns are separated by whitespace and lines - by '\n'.
--(Hint: You need to perform two types of very similar splitting operations: 
--one into lines, and another into column values, for each line)

convert [] = []
convert (x : xs) = (read x :: Integer) : convert xs

parsem :: String -> Matrix
parsem str = map convert $ reverse $ foldl op [] (lines str)
                              where
                                op acc x = (words x) : acc 

--Ex2. Write a function that converts a matrix to a string encoded as illustrated in the previous exercise.
--(Hint: use folds)
--(Hint: test the function show on several different values)
--myfoldr op acc [] = acc
--myfoldr op acc (x:xs) = op x (myfoldr op acc  xs)

toString :: Matrix -> String
toString l = finalstr $ map concatstr $ foldr aplica [] l
                              where
                                aplica x acc = (:) (map show x) acc
                                concatstr [] = "\n"
                                concatstr l@(x:xs) 
                                  |length l > 1 = x ++ " " ++ concatstr xs
                                  |otherwise = x ++ concatstr xs
                                finalstr l = foldr (++) "" l


--Ex3. Add the following to your code. Test the function displaymat.
displaymat = putStrLn.toString

--Matrix operations
--Ex4. Write a function that computes the scalar product with an integer:

vprod :: Integer -> Matrix -> Matrix
vprod v m = map scalar m
              where
                scalar l = map (* v) l

--Ex5. Write a function which adjoins two matrices by extending rows:
hjoin :: Matrix -> Matrix -> Matrix
hjoin m1 m2 = zipWith (++) m1 m2

--Ex6. Write a function which adjoins two matrices by adding new rows:
vjoin :: Matrix -> Matrix -> Matrix
vjoin m1 m2 = m1 ++ m2

--Ex7. Write a function which adds two matrices.
msum :: Matrix -> Matrix -> Matrix
msum [] [] = []
msum (m1:ms1) (m2:ms2) = [zipWith (+) m1 m2 ] ++ (msum ms1 ms2)


--Ex8. Write a function which computes the transposition of a matrix:
tr :: [[a]] -> [[a]]
tr ([]:_) =[]
tr m =  (map head m) : (tr (map tail m))

--Ex9. Write a function which computes the vectorial product of two matrices.
mprod :: Matrix -> Matrix -> Matrix
mprod m1 m2 = map (\line -> map (\col -> foldr (+) 0 (zipWith (*) line col) ) (tr m2) ) m1

mprod2 m1 m2 = map makeline m1
                where
                  makeline li = map makecolumn (tr m2)
                                where
                                  makecolumn cj = foldr (+) 0 (zipWith (*) li cj)

--Image operations
--10. Implement an image-displaying function. Use the image from the Appendix as a test.
type Image = [String] 

--toStringImg l = foldr (++) "" l
toStringImg :: Image -> String
toStringImg [] = []
toStringImg (x:xs) = x ++"\n" ++toStringImg xs
--Add the following function in order to view images in a nicer format:
displaym = putStrLn . toStringImg

--11. Implement a function which flips an image horizontally:

flipH :: Image -> Image 
flipH image =  
  {-
12. Implement a function which flips an image vertically:

flipV :: Image -> Image
flipV = 
13. Implement a function which rotates an image 90grd clockwise

rotate90r :: Image -> Image
rotate90r =
14. Implement a function which rotates an image -90grd clockwise

rotate90l :: Image -> Image
rotate90l = 
15. Implement a function which returns a diamond of a specified height. Example:

                          *
                         ***
   diamond 3 =          *****
                         ***
                          *

                         *
   diamond 2 =          ***
                         *
diamond :: Integer -> Image 
16. Implement a function with takes two images with the same dimensions. The second image is a mask. The output will be another image in which all pixels from the first image overlaying with a '*'-pixel from the mask will be displayed. All others will be deleted (made equal to ' '). Example:

  
   
   ******                ****       ****
   **  **    overlay    ******  =  **  ** 
   **  **                ****       *  *
   ******                 **         **
overlay :: Image -> Image -> Image
More matrices
17. Implement a function which places zeros above the diagonal of a square matrix. Use folds.

18. Implement a function which places zeros below the diagonal of a square matrix. Use folds.

19. Implement either of 17 or 18 using the function(s) take and drop. Check the types of these and test them.

20. Implement a function which computes the diagonal matrix of a square matrix.

21. Implement a function which computes recursively the determinant of a matrix. What additional functions would be necessary?
-}