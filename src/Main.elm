module Main exposing (main)

import Array exposing (Array)
import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html



-- MODEL


rules_text =
    "The goal is to collect more marbles (in your mancala) than your opponent does. The Mancala 'board' is made up of 2 rows of 6 holes each. 4 marbles are placed in each of the 12 holes. The color of the marble is irrelevant. Each player has an empty 'mancala' to the right side of the Mancala board. The game begins with one player picking up all of the marbles in any one of the non-empty holes on his side. Moving counter-clockwise, the player deposits one of the marbles in each hole he runs into until the marbles run out. If you run into your own mancala, deposit one marble in it. If you run into your opponent's mancala, skip it. If the last marble you drop is in your own mancala, you get a free turn. If the last marble you drop is in an empty hole on your side, you empty all marbles on the hole directly opposite to your hole and put it in your hole. The game ends when all the 6 holes on one side of the Mancala board are empty. The Player who still has marbles on his side of the board when the game ends captures all of those marbles and places it in his mancala. Count all the marbles in each mancala. The winner is the Player with the most marbles. The game idea and this rule description is taken from "


rules_text_element =
    Element.textColumn [ spacing 10, padding 10 ]
        [ paragraph []
            [ text rules_text
            , link [ Font.color <| rgb 0 0 1 ]
                { url = "https://www.hackerrank.com/challenges/mancala6"
                , label = text "HackerRank.com."
                }
            ]
        ]


total_num_holes =
    14


initial_num_marbles_per_hole =
    4


type ActivePlayer
    = Player
    | Opponent


type alias GameState =
    { board_state : Array Int
    , active_player : ActivePlayer
    }


type alias Model =
    { game_state : GameState
    , last_game_state : Maybe GameState
    , rules_visible : Bool
    }


initialModel : Model
initialModel =
    let
        initial_board_state =
            let
                delete_player_mancala =
                    Array.set 6 0

                delete_opponent_mancala =
                    Array.set 13 0

                all_full =
                    Array.initialize total_num_holes (always initial_num_marbles_per_hole)
            in
            all_full |> delete_player_mancala |> delete_opponent_mancala

        initial_game_state =
            { board_state = initial_board_state, active_player = Player }
    in
    { game_state = initial_game_state
    , last_game_state = Nothing
    , rules_visible = False
    }


wrapped_int_increment i =
    modBy total_num_holes (i + 1)


wrapped_int_decrement i =
    modBy total_num_holes (i - 1 + total_num_holes)


increment board_state index =
    case Array.get index board_state of
        Nothing ->
            board_state

        Just num_marbles ->
            Array.set index (num_marbles + 1) board_state


steal_marbles game_state winning_hole_index losing_hole_index =
    let
        num_marbles =
            case Array.get losing_hole_index game_state.board_state of
                Just i ->
                    i

                Nothing ->
                    0

        -- cannot happen with proper call
        board_state_empty_hole =
            Array.set losing_hole_index 0 game_state.board_state

        board_state_stealing_performed =
            Array.set winning_hole_index (1 + num_marbles) board_state_empty_hole
    in
    { game_state | board_state = board_state_stealing_performed }


check_for_marbles_steal_bonus game_state index_last_drop =
    let
        stopped_on_own_hole =
            case game_state.active_player of
                Player ->
                    0 <= index_last_drop && index_last_drop <= 5

                Opponent ->
                    7 <= index_last_drop && index_last_drop <= 12

        that_hole_was_empty =
            Just 1 == Array.get index_last_drop game_state.board_state

        opposite_hole_index =
            12 - index_last_drop
    in
    if stopped_on_own_hole && that_hole_was_empty then
        steal_marbles game_state index_last_drop opposite_hole_index

    else
        game_state


switch_player game_state =
    case game_state.active_player of
        Player ->
            { game_state | active_player = Opponent }

        Opponent ->
            { game_state | active_player = Player }


check_for_bonus game_state index_last_drop =
    -- bonus effect = additional move for the player, or stealing marbles from opponent
    case ( index_last_drop, game_state.active_player ) of
        ( 6, Player ) ->
            { active_player = Player, board_state = game_state.board_state }

        ( 13, Opponent ) ->
            { active_player = Opponent, board_state = game_state.board_state }

        _ ->
            check_for_marbles_steal_bonus game_state index_last_drop |> switch_player


drop_marbles game_state marbles_in_hand current_index =
    let
        -- first check if index must be skipped (we do not drop into opponents mancala)
        got_to_skip =
            case ( game_state.active_player, current_index ) of
                ( Player, 13 ) ->
                    True

                ( Opponent, 6 ) ->
                    True

                _ ->
                    False

        index =
            if got_to_skip then
                wrapped_int_increment current_index

            else
                current_index

        -- drop one marble in the next hole
        inc_board_state =
            increment game_state.board_state index

        inc_game_state =
            { game_state | board_state = inc_board_state }
    in
    if marbles_in_hand > 1 then
        drop_marbles inc_game_state (marbles_in_hand - 1) (wrapped_int_increment index)

    else
        check_for_bonus inc_game_state current_index


make_legal_move model index =
    let
        last_game_state =
            Just model.game_state

        num_marbles =
            case Array.get index model.game_state.board_state of
                Just i ->
                    i

                Nothing ->
                    0

        board_state_empty_hole =
            Array.set index 0 model.game_state.board_state

        game_state_empty_hole =
            { board_state = board_state_empty_hole, active_player = model.game_state.active_player }

        new_game_state =
            drop_marbles game_state_empty_hole num_marbles (wrapped_int_increment index)
    in
    { model | game_state = new_game_state, last_game_state = last_game_state }


make_move model index =
    let
        correct_side =
            case model.game_state.active_player of
                Player ->
                    0 <= index && index <= 5

                Opponent ->
                    7 <= index && index <= 12

        hole_non_empty =
            case Array.get index model.game_state.board_state of
                Just i ->
                    i > 0

                Nothing ->
                    False

        legal_move =
            correct_side && hole_non_empty
    in
    if legal_move then
        make_legal_move model index

    else
        model


revert model =
    case model.last_game_state of
        Nothing ->
            model

        Just gs ->
            { model | game_state = gs, last_game_state = Nothing }


change_rule_visibility model =
    { model | rules_visible = not model.rules_visible }



-- UPDATE


type Msg
    = Action Int
    | Revert
    | ChangeRuleVisibility


update : Msg -> Model -> Model
update msg model =
    case msg of
        Action index ->
            make_move model index

        Revert ->
            revert model

        ChangeRuleVisibility ->
            change_rule_visibility model



-- VIEW


green =
    rgb 0.0 1.0 0.0


gray =
    rgb 0.5 0.5 0.5


brown =
    rgb 0.6 0.3 0


append_zero s =
    case String.length s of
        1 ->
            String.append "0" s

        _ ->
            s


view : Model -> Html.Html Msg
view model =
    let
        -- view functions for buttons
        view_single_hole col index =
            let
                num_marbles =
                    case Array.get index model.game_state.board_state of
                        Just i ->
                            i

                        Nothing ->
                            0
            in
            Input.button [ width (px 100), Font.center, padding 30, Border.width 3, Border.rounded 80, Background.color col, Font.size 36 ]
                { onPress = Just (Action index), label = num_marbles |> String.fromInt |> text }

        view_single_hole_player =
            case model.game_state.active_player of
                Player ->
                    view_single_hole green

                Opponent ->
                    view_single_hole gray

        view_single_hole_opponent =
            case model.game_state.active_player of
                Player ->
                    view_single_hole gray

                Opponent ->
                    view_single_hole green

        player_mancala =
            case Array.get 6 model.game_state.board_state of
                Just i ->
                    i

                Nothing ->
                    0

        opponent_mancala =
            case Array.get 13 model.game_state.board_state of
                Just i ->
                    i

                Nothing ->
                    0

        player_B =
            case model.game_state.active_player of
                Player ->
                    row [ padding 3, centerX, Font.size 28 ] [ text "← Player B" ]

                Opponent ->
                    row [ padding 3, centerX, Font.size 28, Background.color green ] [ text "← Player B" ]

        player_A =
            case model.game_state.active_player of
                Player ->
                    row [ padding 3, centerX, Font.size 28, Background.color green ] [ text "Player A →" ]

                Opponent ->
                    row [ padding 3, centerX, Font.size 28 ] [ text "Player A →" ]

        show_rules =
            if model.rules_visible then
                rules_text_element

            else
                Element.none

        board_ui =
            row [ padding 5, Border.width 4, Border.rounded 10, centerY, centerX, spacing 20 ]
                [ -- This is left-to-right board layout
                  el [ width (px 100), height fill, padding 26, Border.width 4, Border.rounded 10, Font.size 36, Font.center, Background.color gray ] (opponent_mancala |> String.fromInt |> text) -- this is the left score
                , column [ spacing 20 ]
                    [ -- this is the two rows of  holes
                      row [ spacing 10 ] (List.range 7 12 |> List.map view_single_hole_opponent |> List.reverse)
                    , row [ spacing 10 ] (List.range 0 5 |> List.map view_single_hole_player)
                    ]
                , el [ width (px 100), alignBottom, height fill, padding 26, Border.width 4, Border.rounded 10, Font.size 36, Font.center, Background.color gray ] (player_mancala |> String.fromInt |> text) -- this is the right score
                ]
    in
    layout [ width fill, height fill ] <|
        column [ padding 15 ]
            [ player_B
            , board_ui
            , player_A
            , row [ padding 5 ] [ Input.button [ Border.width 3, Border.rounded 10, padding 8, Font.size 28 ] { onPress = Just Revert, label = text "Undo last move" } ]
            , row [ padding 5 ] [ Input.button [ Border.width 3, Border.rounded 10, padding 8, Font.size 28 ] { onPress = Just ChangeRuleVisibility, label = text "Show Rules" } ]
            , show_rules
            ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
