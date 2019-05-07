-- Querying & Updating The 50 Bestselling Albums UK
-- Discrete Mathematics and Functional Programming
-- Functional Programming Assignment 2018/19
-- 
--

import System.IO
import Data.List
import Data.Char
import Data.Ord
import Text.Read

__author__ = "00000 <00000@myport.ac.uk>"
__version__ = "1.0.0"

--
-- Types
--
-- The data object that describes a given UK best selling album. Each album’s details include the
-- title, artist, year of release and sales. Deriving the ability to show, equal, show and read.
data Album = Album { title :: String, band :: String, year :: Int, sales :: Int } deriving (Eq,Show,Read) 

-- Test data used to validate that check that the logical functions are doing as expected correctly,
-- without having unneeded complex testing when the introduction of the IO is implemented.
testData :: [Album]
testData =
  [ Album "Greatest Hits"                             "Queen"             1981 6300000
  , Album "Gold: Greatest Hits"                       "ABBA"              1992 5400000
  , Album "Sgt. Pepper's Lonely Hearts Club  Band"    "The Beatles"       1967 5340000
  , Album "21"                                        "Adele"             2011 5110000
  , Album "(What's the Story) Morning Glory?"         "Oasis"             1995 4940000
  , Album "Thriller"                                  "Michael Jackson"   1982 4470000
  , Album "The Dark Side of the Moon"                 "Pink Floyd"        1973 4470000
  , Album "Brothers in Arms"                          "Dire Straits"      1985 4350000
  , Album "Bad"                                       "Michael Jackson"   1987 4140000
  , Album "Rumours"                                   "Fleetwood Mac"     1977 4090000
  , Album "Greatest Hits II"                          "Queen"             1991 3990000
  , Album "Back to Black"                             "Amy Winehouse"     2006 3940000
  , Album "The Immaculate Collection"                 "Madonna"           1990 3700000
  , Album "25"                                        "Adele"             2015 3500000
  , Album "Stars"                                     "Simply Red"        1991 3450000
  , Album "Come On Over"                              "Shania Twain"      1998 3430000
  , Album "x"                                         "Ed Sheeran"        2014 3380000
  , Album "Legend"                                    "Bob Marley"        1984 3380000
  , Album "Bat Out of Hell"                           "Meat Loaf"         1977 3370000
  , Album "Back to Bedlam"                            "James Blunt"       2004 3360000
  , Album "Urban Hymns"                               "The Verve"         1997 3340000
  , Album "Bridge over Troubled Water"                "Simon & Garfunkel" 1970 3260000
  , Album "1"                                         "The Beatles"       2000 3230000
  , Album "Spirit"                                    "Leona Lewis"       2007 3170000
  , Album "Crazy Love"                                "Michael Bublé"     2009 3130000
  , Album "No Angel"                                  "Dido"              2000 3090000
  , Album "White Ladder"                              "David Gray"        1998 3020000
  , Album "The Fame"                                  "Lady Gaga"         2009 2990000
  , Album "Only by the Night"                         "Kings of Leon"     2008 2980000
  , Album "A Rush of Blood to the Head"               "Coldplay"          2002 2960000
  , Album "Talk on Corners"                           "The Corrs"         1997 2960000
  , Album "Spice"                                     "Spice Girls"       1996 2960000
  , Album "Life for Rent"                             "Dido"              2003 2900000
  , Album "Beautiful World"                           "Take That"         2006 2880000
  , Album "The Joshua Tree"                           "U2"                1987 2880000
  , Album "Hopes and Fears"                           "Keane"             2004 2860000
  , Album "The War of the Worlds"                     "Jeff Wayne"        1978 2800000
  , Album "X&Y"                                       "Coldplay"          2005 2790000
  , Album "Jagged Little Pill"                        "Alanis Morissette" 1995 2780000
  , Album "Tubular Bells"                             "Mike Oldfield"     1973 2760000
  , Album "Scissor Sisters"                           "Scissor Sisters"   2004 2760000
  , Album "...But Seriously"                          "Phil Collins"      1989 2750000
  , Album "Tracy Chapman"                             "Tracy Chapman"     1988 2710000
  , Album "Parachutes"                                "Coldplay"          2000 2710000
  , Album "The Man Who"                               "Travis"            1999 2687500
  , Album "Greatest Hits"                             "ABBA"              1975 2606000
  , Album "I've Been Expecting You"                   "Robbie Williams"   1998 2586500
  , Album "Come Away with Me"                         "Norah Jones"       2002 2556650
  , Album "Graceland"                                 "Paul Simon"        1986 2500000
  , Album "Ladies & Gentlemen: The Best of"           "George Michael"    1998 2500000
  ]

--
-- Functional Implementation
--

-- The rightPad function pads the current st ring with a given char (repeated, with the int amount)
-- so that the resulting string reaches a gi ven length. The padding is applied from the end (right)
-- of the current string.
rightPad :: String -> Char -> Int -> String
rightPad input padder amount = input ++ replicate (amount - length input) padder

-- The leftPad function pads the current str ing with another char (repeated, with the int amount)
-- until the resulting string reaches the gi ven length. The padding is applied from the start
-- (left) of the current string.
leftPad :: String -> Char -> Int -> String
leftPad input padder amount = replicate (amount - length input) padder ++ input

-- Creates a duplicate-free version of an list, in which only the first occurrence of each element
-- is kept. The order of result values is determined by the order they occur in the list.
unique :: Eq a => [a] -> [a]
unique (x : xs) = x : unique (filter (/= x) xs)
unique [] = []

-- The method returns the index list after the given index of the array has been removed. If the
-- index is outside the scope of the arrays min and max lengths then the array is returned.
removeByIndex :: Int -> [a] -> [a]
removeByIndex _ [] = []
removeByIndex index (x : xs) 
  | index == 0 = xs
  | otherwise  = x : removeByIndex (index - 1) xs

-- Checks if value is classified as a Integer, this must meet the requirements of a Int, any other
-- type including a double will return this as false.
isOfInteger :: String -> Bool
isOfInteger any = (show (readMaybe any :: Maybe Int)) /= "Nothing"

-- Checks if value is classified as a Double, this must meet the requirements of a Double, any other
-- type including a double will return this as false.
isOfDouble :: String -> Bool
isOfDouble any = (show (readMaybe any :: Maybe Double)) /= "Nothing"

-- Checks if value is classified as a Number, this includes of type integer and double. If they meet
-- either it will return true. Otherwise false.
isOfNumber :: String -> Bool
isOfNumber any = isOfInteger any || isOfDouble any

-- Sorts the provided albums in descending order based on the sales amount of a given album, leading
-- to the album that had the most amount of sales to be at the top while the one with the least at
-- the bottom of the list.
sortAlbumsDescending :: [Album] -> [Album]
sortAlbumsDescending albums = (reverse (sortOn sales albums))

-- Compares two albums title, band and year together to validate if they are equal or not. Not
-- taking into consideration casing.
albumsEqual :: Album -> Album -> Bool
albumsEqual albumOne albumTwo
  | lowerString (band albumOne) /= lowerString (band albumTwo) = False
  | lowerString (title albumOne) /= lowerString (title albumTwo) = False
  | year albumOne /= year albumTwo = False
  | otherwise = True

-- Compares two albums title and band (just names) together to validate if they are equal or not.
-- Not taking into consideration casing.
albumsEqualNames :: Album -> Album -> Bool
albumsEqualNames albumOne albumTwo
  | lowerString (band albumOne) /= lowerString (band albumTwo) = False
  | lowerString (title albumOne) /= lowerString (title albumTwo) = False
  | otherwise = True

-- Returns true if and only if a album already exists in the provided list of albums. This does not
-- care about the base sensitivity of the band title and the band name when doing the compare. This
-- will do the compare lowercase with the expectation that one band will not release two albums with
-- the exact same name.
doesAlbumExist :: Album -> [Album] -> Bool
doesAlbumExist album albums = length (filter (\x -> albumsEqual x album) albums) /= 0

-- Returns true if and only if a album already exists in the provided list of albums. This does not
-- care about the base sensitivity of the band title and the band name when doing the compare.
doesAlbumExistNames :: String -> String -> [Album] -> Bool
doesAlbumExistNames title band albums = length (filter (\x -> albumsEqualNames x album) albums) /= 0
  where album = Album title band 0 0

-- The getAlbumStringSpacing function return ns a tuple of two integer values, these values represent
-- the length of the two longest strings of  a given album. This only takes into consideration the
-- title and band name of the given album l ist. 
getAlbumStringSpacing :: [Album] -> (Int, Int)
getAlbumStringSpacing albums =
  (length (maximumBy (comparing length) [title album | album <- albums]), 
  length (maximumBy (comparing length) [band album | album <- albums]))

-- The albumToString function returns a give n string of the provided album. WIth each value of the
-- album split with the "|" character, additionally applying the first int of the int tuple as a
-- right based padding for the title and second for the right base padding of the band name.
albumToString :: (Int, Int) -> Album -> String
albumToString (titlePad, bandPad) album = 
  intercalate " | " [padTitle, padBand, (show (year album)), (show (sales album))]
 where
  padTitle = rightPad (title album) ' ' titlePad
  padBand = rightPad (band album) ' ' bandPad

-- The albumsToString function returns a single string representing a entire album list after
-- applying the albumToString function to each given album, additionally appending the newline
-- character at the end of each album.
albumsToString :: [Album] -> String
albumsToString albums = 
  intercalate "\n" (map (albumToString albumSpacers) albums)
  where albumSpacers = getAlbumStringSpacing albums

-- Returns a string format of a tuple with right side padding on the first value, this will allow
-- for properly formatted tuples. If used correctly can be used to create nicely formatted tables of
-- tuples.
tupleToString :: Int -> (String, String) -> String
tupleToString padding (first, second) = intercalate " | " [rightPad first ' ' padding, second]

-- Returns a string formatted table style of all the given tuples in the list. Intercalating a new
-- line between each given tuple to correctly keep each one on each level.
tuplesToString :: [(String, String)] -> String
tuplesToString tuples = intercalate "\n" (map (tupleToString tupleSpacer) tuples)
  where tupleSpacer = (length (maximumBy (comparing length) [(show first) | (first, second) <- tuples]))

-- returns the calling string value converted to lower case. Regardless of the value was previously
-- lower or not all values will be iterated over to be lowered.
lowerString :: String -> String
lowerString string = [toLower loweredString | loweredString <- string]

-- Creates a slice of list with the top amount albums based on the sales. Elements are taken until
-- total album's have been selected based on the amount of sales sold. In descending order. It must
-- sort the list based on the sales first as we don't know in which order we will be gathering the
-- sales, so we must make the assumption that the list is not sorted yet.
--
-- Int value used to gather the Nth number of sales by sales sold.
topAlbumsBySales :: Int -> [Album] -> [Album]
topAlbumsBySales amount albums = take amount (sortAlbumsDescending albums)

-- Filters a list of albums between two given years, returning the newly filtered list with all
-- albums that where released between year one and year two respectively. Years are bidirectional
-- and don't have to be put in any real direct order. Meaning that 2008 2000 and 2000 2008 are
-- treated the same.
albumsBetweenYears :: Int -> Int -> [Album] -> [Album]
albumsBetweenYears yearOne yearTwo albums
  | yearOne > yearTwo = albumsBetweenYears yearTwo yearOne albums
  | otherwise = filter (\x -> year x >= yearOne && year x <= yearTwo) albums

-- Filters a list of album's in which the title of the given album starts with the provided prefix.
-- This does not ignore spaces and these must be taken into consideration, it does ignore case
-- though. A list of all albums which start with the provided prefix will be returned.
albumsWithPrefix :: String -> [Album] -> [Album]
albumsWithPrefix prefix albums = filter (\x -> isPrefixOf (lowerString prefix) (lowerString (title x))) albums

-- Gives the total number of sale figures for a given artist/band, this is the total number of sales
-- of the given artist/band of all there albums in the list. This is a direct match regardless of
-- casing. E.g "Queen" and "queen" will be matched.
bandTotalSales :: String -> [Album] -> Int
bandTotalSales name albums = sum [sales x | x <- albums, lowerString (band x) == lowerString name]

-- Gives the total number of times a given band is within the top n number of records, this will
-- order the records by total number of records sold and then take the N top records. The counting
-- will be performed on this list of bands using a lowercase compare of band names.
bandCountInAlbums :: String -> Int -> [Album] -> Int
bandCountInAlbums name top albums = length (filter (\x -> lowerString (band x) == lowerString name) albums)

-- Gives a list of records of the band and the total number of times that band appears in the top N
-- albums sold based on record count, performed on this list of bands using a lowercase compare of
-- band name. Returning a tuple of band name and amount it appears in the top N amount.
bandsInTop :: Int -> [Album] -> [(String, Int)]
bandsInTop top albums = reverse (unique (sortOn snd [(band x, (bandCountInAlbums (band x) top topAlbums)) | x <- topAlbums]))
  where topAlbums = topAlbumsBySales top (take top albums)

-- Removes a band by a given index after sorting the list based on sales. If the position is outside
-- the scope of the list then the list is returned.
removeBandByPosition :: Int -> [Album] -> [Album]
removeBandByPosition position albums = removeByIndex (position - 1) ((topAlbumsBySales (length albums)) albums)

-- Adds a new album to a given list, sorting on sales.
addAlbum :: Album -> [Album] -> [Album]
addAlbum album albums = topAlbumsBySales ((length albums) + 1) (albums ++ [album])

-- Increments the sales of a given album by the provided amount within the list of provided albums.
-- Returns a new updated list of albums with the newly adjusted bands name.
incrementSalesOfAlbum :: String -> String -> Int -> [Album] -> [Album]
incrementSalesOfAlbum albumName bandName amount (x : xs)
  | lowerString (band x) == bName && lowerString (title x) == aName = x { sales = (sales x + amount) } : xs
  | otherwise = x : incrementSalesOfAlbum albumName bandName amount xs
  where
    bName = lowerString bandName
    aName = lowerString albumName

-- Demo function to test basic functionality (without persistence - i.e. testData doesn't change and
-- nothing is saved/loaded to/from albums file).

demo :: Int -> IO ()
demo 1 = putStrLn (albumsToString (sortAlbumsDescending testData))
demo 2 = putStrLn (albumsToString (topAlbumsBySales 10 testData))
demo 3 = putStrLn (albumsToString (albumsBetweenYears 2000 2008 testData))
demo 4 = putStrLn (albumsToString (albumsWithPrefix "Th" testData))
demo 5 = putStrLn ("'Queen' total sales: " ++ show (bandTotalSales "Queen" testData))
demo 6 = putStrLn (tuplesToString [(first, show second) | (first, second) <- bandsInTop 50 testData])
demo 7 = putStrLn (albumsToString (addAlbum (Album "Progress" "Take That" 2010 2700000) (removeBandByPosition 50 testData)))
demo 8 = putStrLn (albumsToString (topAlbumsBySales (length testData) (incrementSalesOfAlbum "21" "Adele" 400000 testData)))
demo _ = putStrLn ("Please enter a valid demo value, 1 to 8.")

--
--
-- Your user interface (and loading/saving) code goes here
--
--

-- Save a list of workstations to the provided file name, this will be inserted into a text file in
-- the haskell data type format. This format will be hard to parse in any other related language.
saveAlbums :: String -> [Album] -> IO ()
saveAlbums fileName albums = length albums `seq` writeFile (fileName ++ ".txt") (show albums)

-- Shows a simple formatted message to let the user know that they are using the selected system
-- with the provided version and loaded amount of albums. Giving user feedback of what is going on,
-- they should now know the application name and version + amount loaded / working with.
showTitleMessage :: Int -> IO ()
showTitleMessage loadedAmount = do
  putStrLn ("==========================================")
  putStrLn ("Albums Integration System - Version: " ++ __version__)
  putStrLn ("Loaded Album: " ++ show (loadedAmount))
  putStrLn ("By: " ++ __author__)
  putStrLn ("==========================================\n")

-- Performs a save and exit, this will write all the provided albums to the disk under "albums-data"
-- which is a text file. Normally we would recall back into the menu loop but this will be avoided
-- here to correctly exit the application.
menuActionSaveExit :: [Album] -> IO () 
menuActionSaveExit albums = do
  putStrLn ("==========================================")
  putStrLn ("Saving " ++ show (length albums) ++ " Albums to albums.txt.")
  saveAlbums "albums" albums
  putStrLn ("Exiting.")
  putStrLn ("==========================================\n")

-- prints a single string representing a entire album list after applying the albumToString function
-- to each given album, additionally appending the newline character at the end of each album.
-- Returning back to the performance loop.
menuActionAllAlbums :: [Album] -> IO ()
menuActionAllAlbums albums = do
    putStrLn ("==========================================")
    putStrLn ((albumsToString (sortAlbumsDescending albums)))
    putStrLn ("==========================================\n")
    performMenuActionLoop albums

-- Asks the given user the amount of top albums they want to show, these will be based around the
-- sales of the artist, if the amount is not a number then the user will be warned amount it not
-- being a number and return to the menu. Otherwise the system will display the top N amount of
-- albums by sales.
menuActionTopBySales :: [Album] -> IO ()
menuActionTopBySales albums = do
  putStrLn ("==========================================")
  putStr ("How many top albums do you want to show? ")
  topToShow <- getLine

  -- Validate and confirm the top amount to show is a valid number and integer before continuing,
  -- letting the user know if its not otherwise / returning to the menu.
  if isOfInteger topToShow && topToShow!!0 /= '-' then do
      putStrLn ("\n" ++ (albumsToString (topAlbumsBySales (read topToShow :: Int) albums)))
    else putStrLn ("\nTop albums amount must be a valid non-negative integer, returning to menu.")

  putStrLn ("==========================================\n")
  performMenuActionLoop albums

-- Asks the user for the first and second year to show the related albums that where released
-- between these two years based ont he release date of the album. If the first or second year is
-- not a valid number (integer) then we warn the user of this action and return them back to the
-- home page.
menuActionBetweenYears :: [Album] -> IO ()
menuActionBetweenYears albums = do
  putStrLn ("==========================================")
  putStr ("What is the first year? ")
  firstYear <- getLine

  -- Validate and confirm the first year is a valid number and integer before continuing, letting
  -- the user know if its not otherwise / returning to the menu.
  if isOfInteger firstYear && firstYear!!0 /= '-' then do
    putStr ("\nWhat is the second year? ")
    secondYear <- getLine

  -- Validate and confirm the second year is a valid number and integer before continuing, letting
  -- the user know if its not otherwise / returning to the menu.
    if isOfInteger secondYear && secondYear!!0 /= '-' then do
      let first = (read firstYear :: Int)
      let second = (read  secondYear :: Int)

      -- print out the albums that where released between the first and second year.
      putStrLn ("\n" ++ (albumsToString (albumsBetweenYears first second albums)))

    else putStrLn ("\nsecond year must be a valid non-negative integer, returning to menu.")
  else putStrLn ("\nfirst year must be a valid non-negative integer, returning to menu.")

  putStrLn ("==========================================\n")
  performMenuActionLoop albums

-- Asks the user for the given prefix to search for against the albums title, this will take into
-- consideration spaces but not case. Printing out all albums which relate to the given prefix.
-- Returning back to the menu after.
menuActionStartsWith :: [Album] -> IO ()
menuActionStartsWith albums = do
  putStrLn ("==========================================")
  putStr ("What is the prefix of the album title? ")
  prefix <- getLine

  -- As long as the prefix is not a empty value the we will use this to attempt to find albums with
  -- the provided prefix, otherwise there will be nothing to display.
  if prefix /= "" then do
    putStrLn ("\n" ++ (albumsToString (albumsWithPrefix prefix albums)))
  else putStrLn ("\nPrefix must be a valid non-empty string, returning to menu.")

  putStrLn ("==========================================\n")
  performMenuActionLoop albums

-- Asks the user for the given band name to search for against the albums band, this will take into
-- consideration spaces but not case. Printing the total band sales within all the albums provided.
-- Returning back to the menu after.
menuActionBandSales :: [Album] -> IO ()
menuActionBandSales albums = do
  putStrLn ("==========================================")
  putStr ("What is the name of the band? ")
  bandName <- getLine

  if bandName /= "" then do
  -- validate and confirm that the band exists before continuing, its better for the user to know
  -- that no band exists than just giving them a blank screen instead, improving useability.
    let bandExists = length (filter (\x -> lowerString (band x) == lowerString bandName) albums) > 0

    if bandExists then do
      putStrLn ("\n'" ++ bandName ++ "' total sales: " ++ show (bandTotalSales bandName albums))
    else putStrLn ("\nNo band exists with the name '" ++ bandName ++ "', returning to menu.")
  else putStrLn ("\nband name must be a valid non-empty string, returning to menu.")

  putStrLn ("==========================================\n")
  performMenuActionLoop albums

-- Asks the user for the given top amount to filter for against the albums position, counting the
-- total amount of times all albums exist in that given range. Printing out all albums in that range
-- with the amount of times it appears. 
menuActionTotalBandAppearances :: [Album] -> IO ()
menuActionTotalBandAppearances albums = do
  putStrLn ("==========================================")
  putStr ("How many top albums do you want to check? ")
  topToShowInput <- getLine

  -- Validate and confirm the top amount to show is a valid number and integer before continuing,
  -- letting the user know if its not otherwise / returning to the menu.
  if isOfInteger topToShowInput && topToShowInput!!0 /= '-'
    && (read topToShowInput :: Int) <= length albums 
    && (read topToShowInput :: Int) > 0 then do
    let topToShow = (read topToShowInput :: Int)

    putStrLn ("\n" ++ tuplesToString [(fst, show snd) | (fst, snd) <- bandsInTop topToShow albums])
    else do 
      putStrLn ("\nTop to show amount must be a valid non-negative integer")
      putStrLn ( "between 1 and " ++ show (length albums) ++ ", returning to menu.")


  putStrLn ("==========================================\n")
  performMenuActionLoop albums

-- Asks the user for the information related to a new album to be created, this includes the title,
-- band, release date and the sales of the album. If for any reason any of  the information is not
-- usable (e.g empty string, negative numbers etc) then it will just return early to the menu.
menuActionAddNewAlbum :: [Album] -> IO ()
menuActionAddNewAlbum albums = do
  putStrLn ("==========================================")
  putStr("What is the title of the album? ")
  title <- getLine

  -- Validate that the title is not empty, if its empty we will end early.
  if title /= "" then do
    putStr("\nWhat is the name of the band of the album? ")
    band <- getLine

  -- Validate that the band is not empty, if its empty we will end early.
    if band /= "" then do
      putStr("\nWhen was the album released? ")
      inReleased <- getLine

      -- Validate that release is a valid number and not negative but also four or more.
      if isOfInteger inReleased && inReleased!!0 /= '-' && length inReleased == 4 then do
        putStr("\nHow many sales does the album have? ")
        inSales <- getLine

        -- Validate that sales is a valid number and not negative.
        if isOfInteger inSales && inSales!!0 /= '-' then do
            -- Parse the release and sales now into a valid integer so that we can create a valid
            -- album. With this we can then validate that it does not already exist and add it to
            -- our current album list.
            let released = (read inReleased :: Int)
            let sales = (read inSales :: Int)

            let album = Album title band released sales
            let updatedAlbums = addAlbum album albums
            
            -- As long as the album does not already exist, go and continue back to the menu with the
            -- newly updated list of albums. In any other failed case we will go and continue back
            -- to the default menu with the starting albums.
            if doesAlbumExist album albums == False then do
              putStr ("\nAdded new album '" ++ title ++ "' (" ++ inReleased ++ ") ")
              putStrLn ("by '" ++ band ++ "' with " ++ inSales ++ " sales")
              putStrLn ("==========================================\n")
              performMenuActionLoop updatedAlbums
            else do
              putStr ("\nThe album '" ++ title ++ "' (" ++ show released ++ ") ")
              putStrLn ("already exists by '" ++ band ++ "', returning to menu.")
              putStrLn ("==========================================\n")
              performMenuActionLoop albums
        else do
          putStrLn ("\nSales must be a valid non-empty integer, returning to menu.")
          putStrLn ("==========================================\n")
          performMenuActionLoop albums
      else do
        putStrLn ("\nRelease must be a valid non-empty integer that has a length of 4, returning to menu.")
        putStrLn ("==========================================\n")
        performMenuActionLoop albums
    else do
      putStrLn ("\nBand must be a non-empty string, returning to menu.")
      putStrLn ("==========================================\n")
      performMenuActionLoop albums
  else do
    putStrLn ("\nTitle must be a non-empty string, returning to menu.")
    putStrLn ("==========================================\n")
    performMenuActionLoop albums

-- Asks the user for the given top amount to filter for against the albums position, counting the
-- total amount of times all albums exist in that given range. Printing out all albums in that range
-- with the amount of times it appears. 
menuActionRemoveByPosition :: [Album] -> IO ()
menuActionRemoveByPosition albums = do
  putStrLn ("==========================================")
  putStr ("What position would you like to remove (ordered by sales)? ")
  positionInput <- getLine

  -- Validate that the position is a valid non-empty number that is also less than the max length of
  -- albums but also greater than 0, keeping the input within the valid range of the albums length
  -- for when the albums are accessed.
  if isOfInteger positionInput && positionInput!!0 /= '-' 
    && (read positionInput :: Int) <= length albums 
    && (read positionInput :: Int) > 0 then do

    let position = (read positionInput :: Int)
    let posIndex = position - 1;

    let removedAlbums = removeBandByPosition position albums
    let removedAlbum = (sortAlbumsDescending albums)!!posIndex

    -- Detailed logging for the user about what album was removed at what index.
    putStr ("\nThe album '" ++ title removedAlbum ++ "' (" ++ show (year removedAlbum) ++ ") ")
    putStrLn ("by band '" ++ band removedAlbum ++ "' removed at position " ++ show position ++ ".")
    putStrLn ("==========================================\n")
    performMenuActionLoop removedAlbums
  else do
    putStr ("\nPosition must be a valid non-negative integer less than or equal to\nthe total albums ")
    putStrLn ("(" ++ show (length albums) ++ ") and greater than 0, returning to menu.")
    putStrLn ("==========================================\n")
    performMenuActionLoop albums

-- Asks the user for the band, title and the increment value that will be used to increment
-- that albums sales by the provided amount, if the title or band i s not valid non-empty strings or
-- the album increment amount is not a valid amount then the action will return to the menu.
menuActionIncrementSales :: [Album] -> IO ()
menuActionIncrementSales albums = do
  putStrLn ("==========================================")
  putStr ("What is the title of the album? ")
  titleInput <- getLine

  putStr ("What is the name of the band? ")
  bandInput <- getLine

  -- First ask for the band input and title before attempting to validate them, this reduces the
  -- amount of indented if statements that we need to do to validate the input, which should improve
  -- readability.
  if titleInput /= "" && bandInput /= "" then do
    if doesAlbumExistNames titleInput bandInput albums then do
      putStr ("How much do you want to increment the sales by? ")
      incrementInput <- getLine

      if isOfInteger incrementInput && incrementInput!!0 /= '-' then do
        let increment = (read incrementInput :: Int)
        let updatedAlbums = incrementSalesOfAlbum titleInput bandInput increment albums

        putStrLn ("\nIncremented '" ++ titleInput ++ "' by '" ++ bandInput ++ "' by " ++ incrementInput ++ ".")
        putStrLn ("==========================================\n")
        performMenuActionLoop updatedAlbums
      else do
        putStrLn ("\nIncrement value must be a valid non-negative integer, returning to menu.")
        putStrLn ("==========================================\n")
        performMenuActionLoop albums
    else do
      putStrLn ("\nAlbum does not exist by title and band, returning to menu.")
      putStrLn ("==========================================\n")
      performMenuActionLoop albums
  else do 
    putStrLn ("\nTitle and Band must be a valid non-empty strings, returning to menu.")
    putStrLn ("==========================================\n")
    performMenuActionLoop albums

-- Called when no match was found for the provided manu action value, quickly letting the user know
-- that no input value exists and then returning to the menu.
menuActionNotFound :: String -> [Album] -> IO ()
menuActionNotFound valueUsed albums = do
  putStrLn ("==========================================")
  putStr("No menu action exists for '" ++ valueUsed ++ "', press any key to return...")
  getLine
  putStrLn ("==========================================\n")
  performMenuActionLoop albums

-- Performs a given action based on the options, if no actions is provided then a formatted error
-- message will be displayed letting the user be aware of what they did wrong and how they can go
-- about fixing it.
menuAction :: String -> [Album] -> IO ()
menuAction "0" albums = menuActionSaveExit albums
menuAction "1" albums = menuActionAllAlbums albums
menuAction "2" albums = menuActionTopBySales albums
menuAction "3" albums = menuActionBetweenYears albums
menuAction "4" albums = menuActionStartsWith albums
menuAction "5" albums = menuActionBandSales albums
menuAction "6" albums = menuActionTotalBandAppearances albums
menuAction "7" albums = menuActionAddNewAlbum albums
menuAction "8" albums = menuActionRemoveByPosition albums
menuAction "9" albums = menuActionIncrementSales albums
menuAction used albums = menuActionNotFound used albums

-- Displays a related user interface menu in a way in which the user can type in a list of possible
-- actions to perform the given action on the loaded albums, with the option to also save and exit
-- if required.
performMenuActionLoop :: [Album] -> IO ()
performMenuActionLoop albums = do
  putStrLn ("==========================================")
  putStrLn ("0. Save & Exit")
  putStrLn ("1. All Albums")
  putStrLn ("2. Top Albums By Sales")
  putStrLn ("3. Albums Between Years")
  putStrLn ("4. Albums That The Title Start With")
  putStrLn ("5. Band Total Sales")
  putStrLn ("6. Total Band Appearances in Top")
  putStrLn ("7. Add New Album")
  putStrLn ("8. Remove Album By Position")
  putStrLn ("9. Increment Album Sales")
  putStrLn ("==========================================\n")
  putStr "What is your option? "
  option <- getLine
  putStr "\n"
  menuAction option albums

-- Entry point of the application for performing command line like actions based around a listed
-- menu of features. These features over the same feature set as all 8 demo methods above but work
-- with live data that can be loaded and written back to disk, persisting data.
main :: IO ()
main = do
  albumsOnDisk <- readFile "albums.txt"
  let albums = read albumsOnDisk :: [Album]
  showTitleMessage (length albums)

  -- -- Show all the loaded albums onto the screen into a column formatted menu structure. Making sure
  -- -- to pad out the columns so there is a clear start and end to each column in the data.
  putStrLn (albumsToString (sortAlbumsDescending albums) ++ "\n")

  -- -- Kick off the menu loop, this will display a meu to the given user with actions they can perform
  -- -- within the given application. These menu actions will be working off the newly loaded in album
  -- -- data.
  performMenuActionLoop albums