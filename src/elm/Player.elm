module Player exposing (takeTurn)

import Warrior exposing (Warrior)
import Warrior.Coordinate exposing (Coordinate)
import Warrior.Direction as Direction exposing (Direction(..))
import Warrior.History as History exposing (History)
import Warrior.Map as Map exposing (Map)
import Warrior.Map.Tile as Tile exposing (Tile)


takeTurn : Warrior -> Map -> History -> Warrior.Action
takeTurn warrior map history =
    let
        -- HISTORY
        stateHistory =
            History.previousStates warrior history

        breadcrumbs =
            stateHistory |> List.map (Tuple.first >> Warrior.position)

        countBreadcrumbs coordinate =
            breadcrumbs |> List.filter ((==) coordinate) |> List.length

        -- HEALTH
        health =
            Warrior.health warrior

        maxHealth =
            Warrior.maxHealth warrior

        -- VISIBLE
        itemAtFeet =
            Map.lookDown warrior map |> Tile.isItem

        whatsInDirection direction =
            Map.look direction warrior map

        directionBreadcrumbs direction =
            whatsInDirection direction
                |> List.head
                |> Maybe.andThen
                    (\( coordinate, tile ) ->
                        if Tile.canMoveOnto tile then
                            Just <| countBreadcrumbs coordinate

                        else
                            Nothing
                    )
                |> Maybe.map (Tuple.pair direction)

        canMove direction =
            whatsInDirection direction
                |> List.head
                |> Maybe.map (Tuple.second >> Tile.canMoveOnto)
                |> Maybe.withDefault False

        canSee : (Tile -> Bool) -> List ( Coordinate, Tile ) -> Bool
        canSee fn see =
            see |> List.map (Tuple.second >> fn) |> List.member True

        canSeeAny : List (Tile -> Bool) -> List ( Coordinate, Tile ) -> Bool
        canSeeAny fnList see =
            fnList |> List.map (\fn -> canSee fn see) |> List.member True

        nextTo : (Tile -> Bool) -> Direction -> Bool
        nextTo fn direction =
            whatsInDirection direction
                |> List.head
                |> Maybe.map (Tuple.second >> fn)
                |> Maybe.withDefault False

        -- ACTIONS
        evade : Direction -> Warrior.Action
        evade direction =
            Direction.all
                |> List.filter canMove
                |> List.head
                |> Maybe.map Warrior.Move
                |> Maybe.withDefault (Warrior.Attack direction)
    in
    --Look down for item and pick it up
    if itemAtFeet then
        Warrior.Pickup

    else
        -- Check for other warriors and attack or evade!
        (Direction.all
            |> List.filter (nextTo Tile.isWarrior)
            |> List.head
            |> Maybe.map
                (\direction ->
                    if health * 2 < maxHealth then
                        evade direction

                    else
                        Warrior.Attack direction
                )
        )
            |> Maybe.withDefault
                -- Check and heal if necessary
                (if health < maxHealth then
                    Warrior.Heal

                 else
                    --Check for and go towards the exit
                    (Direction.all
                        |> List.filterMap
                            (\direction ->
                                if whatsInDirection direction |> canSeeAny [ Tile.isExit, Tile.isItem ] then
                                    Just <| Warrior.Move direction

                                else
                                    Nothing
                            )
                        |> List.head
                    )
                        |> Maybe.withDefault
                            -- Find direction with least breadcrumbs and move that way
                            (Direction.all
                                |> List.filterMap directionBreadcrumbs
                                |> List.sortBy Tuple.second
                                |> List.head
                                |> Maybe.map Tuple.first
                                |> Maybe.withDefault Right
                                |> Warrior.Move
                            )
                )
