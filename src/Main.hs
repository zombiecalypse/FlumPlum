import Control.Applicative
import FRP.Helm
import qualified FRP.Helm.Window as Window
import qualified FRP.Helm.Keyboard as Keyboard
import qualified FRP.Helm.Time as Time
import qualified FRP.Helm.Utilities as Utilities

data Pos = Pos { mx :: Double, my :: Double }
  deriving (Show)

step :: (Int, Int) -> Pos -> Pos
step (dx, dy) pos = Pos <$> ((+realToFrac dx*10) <$> mx)
                        <*> ((+realToFrac dy*10) <$> my) $ pos

render :: (Int, Int) -> Pos -> Element
render (w, h) pos = collage w h [
  square 64 |> filled red |> move (mx pos, my pos)]

-- | Signal that gives the arrows every time the delay signal is caused.
timedArrows :: Signal (Int, Int) -> Signal Double -> Signal (Int, Int)
timedArrows arrows fps = arrows <* fps

main :: IO ()
main = run defaultConfig $ render <$> Window.dimensions <*> stepper
  where state = Pos 0 0
        stepper = foldp step state (timedArrows Keyboard.arrows $ Time.fps 60)
