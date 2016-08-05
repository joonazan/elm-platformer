import Html
import Html.App as App
import Collage
import Element
import AnimationFrame
import Keyboard
import Dict

main = App.program
    { init = (model, Cmd.none)
    , view = view, update = \msg model -> (update msg model, Cmd.none)
    , subscriptions = subscriptions
    }

addPos a b =
    let (x, y) = a
        (x2, y2) = b
    in (x+x2, y+y2)


model =
    { walkers = [{position=(0, 0), yspeed=0, onPlatform=False}]
    , platforms = [{position=(-20, -100), length=200}]
    , keys = Dict.empty
    }

subscriptions model = Sub.batch
    [ AnimationFrame.diffs TimePassed
    , Keyboard.downs KeyDown
    , Keyboard.ups KeyUp
    ]

type Message
    = TimePassed Float
    | KeyUp Keyboard.KeyCode
    | KeyDown Keyboard.KeyCode

update msg model = 
    case msg of
        TimePassed dt ->
            let w2 = maphead (move model.keys dt) model.walkers
            in platformCollision { model | walkers = List.map (fall dt) w2 }
        KeyDown c -> { model | keys = Dict.insert c True model.keys }
        KeyUp c -> { model | keys = Dict.insert c False model.keys }

maphead f list =
    case list of
        head :: tail -> f head :: tail
        [] -> []

move keys dt w =
    let 
        keyPressed c = Maybe.withDefault False (Dict.get c keys)
        direction =
            if keyPressed 37 then -1
            else if keyPressed 39 then 1
            else 0
        yspeed =
            if keyPressed 38 && w.onPlatform then w.yspeed + 13
            else w.yspeed
    in { w | position = addPos w.position (0.2*dt*direction, 0), yspeed = yspeed }

fall dt w =
    let newSpeed = w.yspeed - dt * 0.04
    in { w | yspeed = newSpeed
    , position = addPos w.position (0, newSpeed)
    }

platformCollision model =
    let collide w = List.foldr fix w model.platforms
        fix p w = 
            let (wx, wy) = w.position
                (pstart, py) = p.position
                pend = pstart+p.length
                penetration = py - wy
            in
                if pstart < wx && wx < pend && penetration > 0 && penetration < 10 then
                    { position = (wx, py), yspeed = 0, onPlatform = True }
                else w
        removeOnPlatform w = { w | onPlatform = False}
    in { model | walkers = List.map (removeOnPlatform >> collide) model.walkers }

view model = 
    let line start end = Collage.traced Collage.defaultLine (Collage.segment start end)
        walker w = line w.position (addPos w.position (0, 100))
        platform p = line p.position (addPos p.position (p.length, 0))
    in Element.toHtml <| Collage.collage 500 500 <| (List.map walker model.walkers) ++ (List.map platform model.platforms)