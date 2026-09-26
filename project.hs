import Data.Monoid (Sum(..))
import qualified Data.Map as Map
import Data.Maybe

data Genre = Rap | Pop | IndieFolk | Phonk | SoundCloudRap | Electronic
    deriving (Show, Eq, Enum, Bounded)

data Track = Track{ title :: String, artist :: String, genre :: Genre, duration :: Int, isFavorite :: Bool}
    deriving (Show, Eq)

track1 = Track{title="testTrack1", artist="testArtist", genre=Rap, duration=1, isFavorite=False}
track2 = Track{title="testTrack2", artist="testArtist", genre=IndieFolk, duration=2, isFavorite=False}
track3 = Track{title="testTrack3", artist="testArtist1", genre=Pop, duration=3, isFavorite=True}


mood :: Genre -> String
mood Rap = "energetic"
mood Pop = "easy"
mood IndieFolk = "cozy"
mood Phonk = "hard"
mood SoundCloudRap = "dark"
mood Electronic = "groovy"


shiftGenre :: Int -> Genre -> Genre
shiftGenre n g =
  let genres   = [minBound .. maxBound] :: [Genre]
      len      = length genres
      idx      = fromEnum g
      newIdx   = (idx + n) `mod` len
  in toEnum newIdx


totalDuration :: [Track] -> Int
totalDuration tracks = getSum (foldMap (Sum . duration) tracks)

testDuration :: [Track]
testDuration = [track1, track2, track3]


buildPlaylist :: Map.Map String Track -> [String] -> Maybe [Track]
buildPlaylist library titles = mapM (`Map.lookup` library) titles

lib = Map.fromList [("testTrack1", track1), ("testTrack3", track3)]



formatTime :: Int -> String
formatTime totalSecs =
    let mins = totalSecs `div` 60
        secs = totalSecs `mod` 60
    in show mins ++ ":" ++ if secs < 10 then "0" ++ show secs else show secs

main :: IO ()
main = do
    let mainTrack1 = Track{title="testTrack1", artist="testArtist1", genre=Rap, duration=120, isFavorite=False}
    let mainTrack2 = Track{title="testTrack2", artist="testArtist2", genre=IndieFolk, duration=9, isFavorite=False}
    let mainTrack3 = Track{title="testTrack3", artist="testArtist3", genre=Pop, duration=167, isFavorite=True}
    let mainTrack4 = Track{title="testTrack4", artist="testArtist4", genre=Phonk, duration=100, isFavorite=True}
    let mainTrack5 = Track{title="testTrack5", artist="testArtist5", genre=SoundCloudRap, duration=124, isFavorite=True}
    let mainTrack6 = Track{title="testTrack6", artist="testArtist6", genre=Pop, duration=274, isFavorite=True}

    let mainLib = Map.fromList [("testTrack1", mainTrack1), ("testTrack2", mainTrack2),("testTrack3", mainTrack3), ("testTrack4", mainTrack4),("testTrack5", mainTrack5), ("testTrack6", mainTrack6)]

    putStrLn "Enter track titles separated by spaces:"
    input <- getLine
    let titles = words input
    case buildPlaylist mainLib titles of
        Just tracks -> do
            putStrLn "\n"
            mapM_ (\t -> putStrLn $ title t ++ " - " ++ artist t) tracks
            putStrLn $ "Total: " ++ formatTime (totalDuration tracks)

        Nothing -> do
            putStrLn "\nMissing tracks:"
            let missing = filter (\t -> Map.notMember t lib) titles
            mapM_ putStrLn missing