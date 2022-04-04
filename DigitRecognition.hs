module DigitRecognition where
import Data.List (nub, sort)
import Data.List.Split (chunksOf)
import Data.Tuple (swap)
import Data.Ratio (numerator, denominator)
import Data.Ratio ((%))
import Debug.Trace

-- All undefined values and functions should be completed. Your code will compile and test 
-- (with the -- test flag) even if some functions are left undefined.

--                                          Type Aliases
-- These type aliases help to abstract our code. You will use them extensively in DigitRecognition.hs
--
type PixelImage = [[Bool]] 
-- A pixel image is a two-dimensional list of booleans.
-- False represents an empty pixel, and True a grey or black pixel. Each pixel image will be 28x28.
type Feature = Int
-- Each pixel location is considered a separate feature for classification. Because the image is
-- 28x28, there are 784 total features.
type Digit = Integer
-- Each image is of a specific digit, 0 through 9. To distinguish the labels or guesses from
-- other numbers, we use a type alias.
type Summary = [[(Int, Int)]]
-- number of times a pixel was present, number of times it was absent
type DigitSummary = [(Digit, Summary)]
type DigitCount = [(Digit, Int)]


--                                      Primitive Functions
-- These functions will be used in your implementation of the classifier. Be
-- sure you understand how they are used, but you do not have to understand how they work.
--

--hasFeature checks if an image has a specific feature: i.e. if that pixel is white or blank.
--
--This encapsulates the ugliness of storing images as nested lists. Notice the normally
--forbidden use of !!. This suggests that there should be a better way to handle and process
--images. For the purposes of this project we will accept this.  We can take reassurance that
--lists of length 28 are so short that better storage methods are probably unnecessary.
hasFeature :: PixelImage -> Feature -> Bool
hasFeature img ftr = 
    let dim = length img
        row = img !! (ftr `div` dim)
        pixel = row !! (ftr `mod` dim)
    in pixel
-- Example:    img `hasFeature` ftr

getFeature :: Summary -> Feature -> (Int, Int)
getFeature sum ind = 
    let row = sum !! (ind `div` 28)
        val = row !! (ind `mod` 28)
    in val

--outOf wraps around Haskell's built-in Rational data type. Rationals store fractional values
--precisely, with no possibility of underflow. Internally, the numerator and denominator are
--kept as Integers, which have no maximum outside the space limitations of computer memory. You
--will use this function to return an estimated probability. 
outOf :: Int -> Int -> Rational
outOf a b =  (fromIntegral a) % (fromIntegral b)
--Example:      2 `outOf` 10
--              (length [1..3]) `outOf` (length [1..10])
   

--                                       Milestone One

-- Create a list of all possible features, starting at 0.
allFeatures :: [Feature]
allFeatures = [0..783]

-- Create a list of all possible digit labels. 
allDigits :: [Digit]
allDigits = [0..9]

-- showPixelImage should take a PixelImage and turn it into a single string.
-- Since we have lost gray colors (see readPixelImage in Framework.hs), our
-- string will have '#' for black pixels, and ' ' for white pixels.
--
-- I suggest a helper function that takes an individual row of a pixel image and turns it into a
-- string. You can then use the built-in (unlines) function, which takes a list of strings and
-- turns them into a single string, separated by newline.
-- 
-- Example: showPixelImage [[True, True], [True, False]]
--          "##\n# \n"
pixelize :: [Bool] -> [Char]
pixelize line = [if (bool == True) then '#' else ' ' | bool <- line]

showPixelImage :: PixelImage -> String
showPixelImage img =
    unlines [pixelize line | line <- img]

-- lookupVal takes a key of type a, an association list from a to b, and returns the hopefully
-- unique value associated with the key. If lst contains the tuple (k, v), then 
-- lookupVal k lst should return v.
--
-- Full Credit - Implementation Details:
--   For full credit, ensure that the key matches exactly one tuple in the list. If not, throw an
--   error.
-- Remember: work on full credit until after you've completed the core project!
-- 
-- Example: lookupVal 'b' [('a', 8), ('b', 7), ('c', 9)]
--          7
lookupVal :: Eq a => a -> [(a, b)] -> b
lookupVal key lst
    | length newlst /= 1 = error "Cannot find exact match"
    | otherwise = head newlst
    where newlst = [b | (a,b) <- lst, a == key]
   
--                                       Milestone Two

-- A corpus is an association list between digits and the images that are labeled with that
-- digit. By storing the information this way, we avoid the frequent computation of which images
-- are labeled with which digit. 
--type Corpus = [(Digit, [PixelImage])]
type Corpus = (DigitCount, DigitSummary)

-- When we read in the files, we get a list of image-label tuples. It is far more efficient to
-- group the images by their label as a Corpus. buildCorpus takes the list of tuples and
-- separates it into sub-lists for each label. Order does not matter, either of the digits or of
-- the images associated with each digit.
--
-- I suggest a helper function that takes a digit and returns the list of all images labeled with
-- that digit.
--
-- Full Credit - Implementation Details:
--    For full credit, only create entries in the Corpus for digits that actually have associated
--    images. You might need to use the (nub) function, which returns the set version of a list
--    (i.e.  all duplicate elements have been removed). 
--    Your function must still run quickly!
-- Remember: work on full credit until after you've completed the core project!
-- 
-- Example:  
--imgA = [[True, False]]
--imgB = [[False, False]]
--imgC = [[False, True]]
--imgLbls = [(imgA, 9), (imgB, 2), (imgC, 9)]
-- buildCorpus imgLbls 
--           [(9, [ [[True, False]], [[False, True]] ]), (2, [[[False, False]]])]

featureGrid :: [a] -> [[a]]
featureGrid lst = chunksOf 28 lst


buildSummary :: [PixelImage] -> Summary
buildSummary imgs = 
    let listOfFeatures ftr = [if hasFeature img ftr then 1 else 0 | img <- imgs]
        --listOfFeaturesImg img = [if hasFeature img ftr then 1 else 0 | ftr <- allFeatures]
        --imgFeatureGrid = [(a, (length imgs) - a) | a <- zipLists (listOfFeaturesImg img)]
        --listOfNonFeatures ftr = [if not (hasFeature img ftr) then 1 else 0 | img <- imgs]
        --absentSummaries = [sum (listOfNonFeatures ftr) | ftr <- allFeatures]
        --totalSummary = zip positiveSummaries absentSummaries
        -- absentSummaries = [[if not (hasFeature img ftr) then 1 else 0 | ftr <- allFeatures] | img <- imgs]
    in featureGrid [(sumLst, length lst - sumLst) | ftr <- allFeatures, let lst = listOfFeatures ftr, let sumLst = sum lst]
    --in [(sum [x | (x,_) <- imageSummary img], sum [y | (_,y) <- imageSummary img]) | img <- imgList]


buildCorpus :: [(PixelImage, Digit)] -> Corpus
buildCorpus imgLbls = 
    -- compute and store the number of times a feature was present, absent
    -- returns (DigitCount [(Digit, Int)], DigitSummary [(Digit, Summary)])
    -- summary is of type [[(Int, Int)]]
    let buildList dgt lbls = [img | (img, num) <- lbls, num == dgt]
        allDigitList = [(dgt, buildLength, buildSummary build) | dgt <- allDigits, let build = buildList dgt imgLbls, let buildLength = length build, buildLength > 0]
        --originalCorpus = [(dgt, buildList dgt imgLbls) | dgt <- allDigits]
        -- Do not change anything above this line 
    in ([(dgt, buildLength) | (dgt, buildLength, _) <- allDigitList], [(dgt, buildSummary) | (dgt, _, buildSummary) <- allDigitList])
        -- [(dgt, build) | dgt <- allDigits, let build = buildList dgt imgLbls, length build > 0]

--
--                                  Core Project 

-- Given a corpus and a specific digit Y, probOfDigit estimates P(Y). This is the fraction
-- of the images in the corpus that are labeled with Y.  You will need to use `outOf` to create
-- the fraction.
-- You may find the (sum) function helpful: it takes a list of numbers and adds them together.
-- Example: probOfDigit corpus 9
--         2 % 3
probOfDigit :: Corpus -> Digit -> Rational
probOfDigit (digitCountList, digitSummaryList) digit = 
    lookupVal digit digitCountList `outOf` sum [snd digitCount | digitCount <- digitCountList]
    --lookupVal digit corpus `outOf` (sum [length imageSet | (dgt, imageSet) <- corpus])
    --where digitList = length (lookupVal digit corpus)

-- Given the list of images (imgs) labeled for a given digit Y, and a feature F (ftr),
-- probOfFeature imgs ftr estimates the probability P(ftr=Black | Y). See the assignment page for
-- details.
probOfFeature :: Summary -> Feature -> Rational
probOfFeature summary ftr = 
    let (present, absent) = getFeature summary ftr
    in (1 + present) `outOf` (2 + present + absent)
    -- Feature is just an int, PixelImage is [[bool]], true means black
    --let features = [1 | img <- imgs, hasFeature img ftr]
    --in outOf (if (sum features <= 0) then (sum features) + 1 else sum features) (1 + length imgs)

-- Given the list of images (imgs) labeled for a given digit Y, and a feature F (ftr),
-- probOfNOFeature imgs ftr estimates the probability P(ftr=White | Y). See the assignment page
-- for details.
probOfNoFeature :: Summary -> Feature -> Rational
probOfNoFeature summary ftr = 
    let (present, absent) = getFeature summary ftr
    in (1 + absent) `outOf` (2 + present + absent)
    --let noFeatures = [1 | img <- imgs, not (hasFeature img ftr)]
    --in outOf (if (sum noFeatures <= 0) then (sum noFeatures) + 1 else sum noFeatures) (1 + length imgs)

-- rankOfDigit should estimate the rank of a given digit for a given instance, as specified on
-- the assignment page.
-- You will need to use both probOfDigit, probOfFeature, and probOfNoFeature. 
-- You may find the (product) function helpful.
-- I recommend you calculate the values for positive features (those that occur in newImg)
-- and negative features (those that do not occur in newImg) separately.
rankOfDigit :: Corpus -> Digit -> PixelImage -> Rational
rankOfDigit corpus digit newImg =
    let (digitCount, digitSummary) = corpus
        summaryForDigit = lookupVal digit digitSummary
        --imgsOfDigit = [img | (dgt, imgSet) <- corpus, img <- imgSet, dgt == digit]
        featureProbList = [if (hasFeature newImg ftr) then (probOfFeature summaryForDigit ftr) else (probOfNoFeature summaryForDigit ftr) | ftr <- allFeatures]
    in (product featureProbList) * (probOfDigit corpus digit)

-- classifyImage should return the most likely digit, based on the rank computed by rankOfDigit.
-- You will need to use the maximum function.
-- An important fact: if you have a tuple of two values, maximum returns based on the first
-- value.
-- Extra Style - Implementation Details:
--   Once you have smoothing working, the ideal implementation will check that the maximum rank is
--   greater than 0. If it is not, you should print an error message. However, you will get errors
--   until smoothing is working correctly, so don't insert that check until then! 
--   This is not worth any points!
classifyImage :: Corpus -> PixelImage -> Digit
classifyImage corpus newImg = 
    let digitRanks = [(rankOfDigit corpus digit newImg, digit) | digit <- allDigits]
        maxRank = maximum digitRanks
    in snd maxRank

--                                  Optional Helpful Functions
-- These functions are optional, but may be helpful with debugging. They are not worth any points.

-- valueOfRank takes a rank and turns it into a somewhat reasonable integer, suitable for
-- printing. The ranks may be negative, that's perfectly fine.
valueOfRank :: Rational -> Int
valueOfRank r = 350 + ratLog r 
    where numDigits x = length $ show x
          ratLog r = (numDigits $ numerator r) - (numDigits $ denominator r)


-- rankImage is similar to classify image, but instead of returning the best digit, it should
-- return the list of digits and their ranks. Used by the --ranking flag.
-- It is helpful, but not necessary, to sort the list.
rankImage :: Corpus -> PixelImage -> [(Digit, Int)]
rankImage corpus newImg = 
    undefined
