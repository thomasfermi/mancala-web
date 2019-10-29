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
    "The goal is to collect more marbles (in your mancala) than your opponent does. The Mancala 'board' is made up of 2 rows of 6 holes each. 4 marbles are placed in each of the 12 holes. Each player has an empty 'mancala' to the right side of the Mancala board. The game begins with one player picking up all of the marbles in any one of the non-empty holes on his or her side. Moving counter-clockwise, the player deposits one of the marbles in each hole she or he runs into until the marbles run out. If you run into your own mancala, deposit one marble in it. If you run into your opponent's mancala, skip it. If the last marble you drop is in your own mancala, you get a free turn. If the last marble you drop is in an empty hole on your side, you empty all marbles on the hole directly opposite to your hole and put it in your hole. The game ends when all the 6 holes on one side of the Mancala board are empty. The player who still has marbles on his side of the board when the game ends captures all of those marbles and places it in his mancala. Count all the marbles in each mancala. The winner is the player with the most marbles. Note: The game idea and this rule description is taken from "


rules_text_element =
    Element.textColumn [ spacing 10, padding 10 ]
        [ paragraph [ width <| px 895, Font.size 20, Font.justify ]
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


type GameType
    = Player_vs_Player
    | Player_vs_AI


type ActivePlayer
    = Player
    | Opponent


type alias GameState =
    { board_state : Array Int
    , active_player : ActivePlayer
    }


type alias Model =
    { game_type : GameType
    , game_state : GameState
    , last_game_state : Maybe GameState
    , rules_visible : Bool
    }


init_model : GameType -> Model
init_model game_type =
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
    { game_type = game_type
    , game_state = initial_game_state
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


get_score_for_player player game_state =
    case player of
        Player ->
            Array.slice 0 7 game_state.board_state |> Array.toList |> List.sum

        Opponent ->
            Array.slice 7 14 game_state.board_state |> Array.toList |> List.sum


is_game_state_final game_state = --TODO: refactor needless computations away
    let
        get_num_marbles_in_all_holes_for player =
            case player of
                Player ->
                    Array.slice 0 6 game_state.board_state |> Array.toList |> List.sum

                Opponent ->
                    Array.slice 7 13 game_state.board_state |> Array.toList |> List.sum

        player_marbles =
            get_num_marbles_in_all_holes_for Player

        opponent_marbles =
            get_num_marbles_in_all_holes_for Opponent
    in
        if game_state.active_player == Player then player_marbles == 0 else opponent_marbles == 0


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
        check_for_bonus inc_game_state index


make_move game_state index =
    let
        num_marbles =
            case Array.get index game_state.board_state of
                Just i ->
                    i

                Nothing ->
                    0

        board_state_empty_hole =
            Array.set index 0 game_state.board_state

        game_state_empty_hole =
            { board_state = board_state_empty_hole, active_player = game_state.active_player }
    in
    drop_marbles game_state_empty_hole num_marbles (wrapped_int_increment index)


is_legal_move game_state index =
    let
        correct_side =
            case game_state.active_player of
                Player ->
                    0 <= index && index <= 5

                Opponent ->
                    7 <= index && index <= 12

        hole_non_empty =
            case Array.get index game_state.board_state of
                Just i ->
                    i > 0

                Nothing ->
                    False
    in
    correct_side && hole_non_empty



-- Player is maximizing player and Opponent is minimizing player


heuristic game_state =
    get_score_for_player Player game_state - get_score_for_player Opponent game_state



{- return a list of pairs. Each pair consists of a game state s' and an action a,
   whereby s' is a successor of the current game state s under a (plus potential bonus moves, which are not part of the return).
   let's say the following sequences of actions are possible (sequence because bonus move)
   [0,5], [0,4], [1], [4]. Function returns [(s1,0), (s2,0), (s3,1), (s4,4)], where s1 is the state after [0,5], s2 state after [0,4] etc.
-}


get_all_children_and_actions : GameState -> List ( GameState, Int )
get_all_children_and_actions game_state =
    let
        hole_range =
            case game_state.active_player of
                Player ->
                    ( 0, 5 )

                Opponent ->
                    ( 7, 12 )

        s =
            Tuple.first hole_range

        e =
            Tuple.second hole_range

        -- Create for holes 0,1,2,... the lists: [], [s1,s2], [s3], ... Each list shows reachable states
        list_of_list_of_states =
            List.map (explore_all_reachable_states game_state) (List.range s e)

        -- Map [], [s1,s2], [s3] to (0,[]), (1,[s1,s2]), (2,[s3])
        offset =
            if game_state.active_player == Player then
                0

            else
                7

        list_with_applied_tupling =
            List.indexedMap (\index state -> ( index + offset, state )) list_of_list_of_states

        -- Map (0,[]), (1,[s1,s2]), (2,[s3]) to (1,[s1,s2]), (2,[s3])
        filter_func x =
            Tuple.second x |> List.isEmpty |> not

        filtered_list_with_applied_tupling =
            List.filter filter_func list_with_applied_tupling

        -- Map (1,[s1,s2]), (2,[s3]) to (s1,1) (s2,1), (s3,2)
        expansion_func ( i, l ) =
            List.map (\x -> ( x, i )) l

        result =
            List.concatMap expansion_func filtered_list_with_applied_tupling
    in
    result



--The above line yields List with 6 elements. The i-th element is the list of possible game_states the player can reach after picking hole i.


explore_all_reachable_states : GameState -> Int -> List GameState
explore_all_reachable_states game_state hole =
    --TODO: rewrite for tail recursion optimization
    {- Find all states that can be reached if the player chooses to pick hole=hole.
       Result can be
        - empty list (picking that hole is illegal).
        - list with one element (picking hole is legal, and we get no extra move)
        - list with several moves (player exploiting bonus move rule)
    -}
    if is_legal_move game_state hole then
        let
            resulting_game_state =
                make_move game_state hole
        in
        if resulting_game_state.active_player /= game_state.active_player then
            [ resulting_game_state ]

        else
            let
                hole_range =
                    case game_state.active_player of
                        Player ->
                            ( 0, 5 )

                        Opponent ->
                            ( 7, 12 )

                s =
                    Tuple.first hole_range

                e =
                    Tuple.second hole_range

                list_of_reachable_states =
                    List.concatMap (explore_all_reachable_states resulting_game_state) (List.range s e)
            in
            resulting_game_state :: list_of_reachable_states

    else
        []


take_max : List ( GameState, Int ) -> Int -> Int -> Int -> Int -> Int
take_max children depth alpha beta value =
    let
        v =
            case children of
                [] ->
                    value

                c :: cs ->
                    max value (alpha_beta (Tuple.first c) (depth - 1) alpha beta)

        new_alpha =
            max alpha v
    in
    if beta <= new_alpha then
        v

    else
        case children of
            c :: [] ->
                v

            c :: cs ->
                take_max cs depth alpha beta v

            [] ->
                v



-- should never occur


take_min : List ( GameState, Int ) -> Int -> Int -> Int -> Int -> Int
take_min children depth alpha beta value =
    let
        v =
            case children of
                [] ->
                    value

                c :: cs ->
                    min value (alpha_beta (Tuple.first c) (depth - 1) alpha beta)

        new_beta =
            min beta v
    in
    if new_beta <= alpha then
        v

    else
        case children of
            c :: [] ->
                v

            c :: cs ->
                take_min cs depth alpha beta v

            [] ->
                v



-- should never occur


alpha_beta : GameState -> Int -> Int -> Int -> Int
alpha_beta game_state depth alpha beta =
    -- returns value of game_state
    if depth == 0 || is_game_state_final game_state then
        heuristic game_state

    else
        case game_state.active_player of
            Player ->
                -- maximizing player
                take_max (get_all_children_and_actions game_state) depth alpha beta -9999

            Opponent ->
                -- minimizing player
                take_min (get_all_children_and_actions game_state) depth alpha beta 9999


move_decision_ai game_state max_depth =
    let
        children_actions =
            get_all_children_and_actions game_state

        values_actions =
            List.map (\( gs, a ) -> ( alpha_beta gs max_depth -9999 9999, a )) children_actions

        get_best_move remaining_values_actions best_value_action_so_far =
            case remaining_values_actions of
                [] ->
                    Tuple.second best_value_action_so_far

                va :: vas ->
                    if Tuple.first va < Tuple.first best_value_action_so_far then
                        get_best_move vas va

                    else
                        get_best_move vas best_value_action_so_far
    in
        get_best_move values_actions ( 9999, -1 )



-- responses to messages:


perform_legal_user_action model index =
    let
        last_game_state =
            Just model.game_state

        new_game_state =
            make_move model.game_state index
    in
    { model | game_state = new_game_state, last_game_state = last_game_state }


perform_user_action model index =
    if is_legal_move model.game_state index then
        perform_legal_user_action model index

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


ask_ai_to_move model =
    if is_game_state_final model.game_state then model
    else
        case model.game_state.active_player of
            Player ->
                model

            Opponent ->
                let
                    index =
                        move_decision_ai model.game_state 2
                in
                perform_legal_user_action model index

game_over_clean_up model =
    let 
        playerA_score = get_score_for_player Player model.game_state
        playerB_score = get_score_for_player Opponent model.game_state
        final_board = Array.initialize total_num_holes (always 0)
                      |> Array.set 6 playerA_score 
                      |> Array.set 13 playerB_score
        old_game_state = model.game_state
        final_game_state = {old_game_state | board_state = final_board}
    in 
        {model | game_state = final_game_state}
    

-- UPDATE


type Msg
    = Action Int
    | Revert
    | ChangeRuleVisibility
    | AskAI
    | NewGamePvP
    | NewGamePvAI
    | GameOverCleanUp


update : Msg -> Model -> Model
update msg model =
    case msg of
        Action index ->
            perform_user_action model index

        Revert ->
            revert model

        ChangeRuleVisibility ->
            change_rule_visibility model

        AskAI ->
            ask_ai_to_move model

        NewGamePvP ->
            init_model Player_vs_Player

        NewGamePvAI ->
            init_model Player_vs_AI

        GameOverCleanUp ->
            game_over_clean_up model



-- VIEW


green =
    rgb 0.0 1.0 0.0


gray =
    rgb 0.6 0.6 0.6


white =
    rgb 1.0 1.0 1.0


view : Model -> Html.Html Msg
view model =
    let
        -- view functions for buttons
        view_single_hole col clickable index =
            let
                num_marbles =
                    case Array.get index model.game_state.board_state of
                        Just i ->
                            i

                        Nothing ->
                            0

                on_press_event =
                    if clickable then
                        Just (Action index)

                    else
                        Nothing
            in
            Input.button [ width (px 100), Font.center, padding 30, Border.width 3, Border.rounded 80, Background.color col, Font.size 36 ]
                { onPress = on_press_event, label = num_marbles |> String.fromInt |> text }

        view_single_hole_player =
            case model.game_state.active_player of
                Player ->
                    view_single_hole green True

                Opponent ->
                    view_single_hole gray True

        view_single_hole_opponent =
            case model.game_type of
                Player_vs_Player ->
                    case model.game_state.active_player of
                        Player ->
                            view_single_hole gray True

                        Opponent ->
                            view_single_hole green True

                Player_vs_AI ->
                    view_single_hole gray False

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
            case model.game_type of
                Player_vs_Player ->
                    case model.game_state.active_player of
                        Player ->
                            row [ padding 6, centerX, Font.size 28 ] [ text "← Player B" ]

                        Opponent ->
                            row [ padding 6, centerX, Font.size 28, Background.color green ] [ text "← Player B" ]

                Player_vs_AI ->
                    case model.game_state.active_player of
                        Player ->
                            row [ padding 6, centerX, Font.size 28 ] [ text "← AI opponent" ]

                        Opponent ->
                            row [ centerX ]
                                [ Input.button [ Border.width 3, Border.rounded 10, padding 3, Font.size 28, Background.color green ]
                                    { onPress = Just AskAI, label = text "← AI opponent" }
                                ]

        player_A =
            row [ padding 6, centerX, Font.size 28 ] [ text "Player A →" ]

        {- case model.game_state.active_player of
           Player ->
               row [ padding 6, centerX, Font.size 28, Background.color green ] [ text "Player A →" ]

           Opponent ->
               row [ padding 6, centerX, Font.size 28 ] [ text "Player A →" ]
        -}
        show_rules =
            if model.rules_visible then
                rules_text_element

            else
                Element.none

        show_rules_button =
            row [ padding 5 ] [ Input.button [ Border.width 3, Border.rounded 10, padding 8, Font.size 28 ] { onPress = Just ChangeRuleVisibility, label = text "Show Rules" } ]

        undo_last_move_button =
            row [ padding 5 ] [ Input.button [ Border.width 3, Border.rounded 10, padding 8, Font.size 28 ] { onPress = Just Revert, label = text "Undo last move" } ]

        button_pvp =
            let
                button_pvp_helper col =
                    row [ alignLeft ] [ Input.button [ Font.size 28, Border.width 3, Border.rounded 10, padding 8, Background.color col ] { onPress = Just NewGamePvP, label = text "Player vs. Player game" } ]
            in
            case model.game_type of
                Player_vs_Player ->
                    button_pvp_helper gray

                Player_vs_AI ->
                    button_pvp_helper white

        button_pvai =
            let
                button_pvai_helper col =
                    row [ alignRight ] [ Input.button [ Font.size 28, Border.width 3, Border.rounded 10, padding 8, Background.color col ] { onPress = Just NewGamePvAI, label = text "Player vs. AI game" } ]
            in
            case model.game_type of
                Player_vs_Player ->
                    button_pvai_helper white

                Player_vs_AI ->
                    button_pvai_helper gray

        button_game_over =
            let 
                marble_sum =  (Array.slice 7 13 model.game_state.board_state |> Array.toList |> List.sum)
                            + (Array.slice 0 6 model.game_state.board_state |> Array.toList |> List.sum)
                button_text = 
                    if marble_sum == 0
                    then 
                        case heuristic model.game_state of 
                            0 -> "It's a draw!"
                            x -> if x > 0 then "Player A won!"
                                 else 
                                    case model.game_type of 
                                        Player_vs_AI -> "The AI won!"
                                        Player_vs_Player -> "Player B won!"
                    else "Game over. Press here to clean up!"
            in 
            if is_game_state_final model.game_state 
            then row [ alignRight ] [ Input.button [ Font.size 28, Border.width 3, Border.rounded 10, padding 8, Background.color green ] { onPress = Just GameOverCleanUp, label = text button_text } ]
            else Element.none

        game_type_selection_bar =
            row [ padding 2, width fill, Font.size 28, Border.width 3 ]
                [ button_pvp
                , button_pvai
                ]

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
            [ game_type_selection_bar
            , row [ padding 10 ] [] -- some vertical space
            , player_B
            , board_ui
            , player_A
            , row [ padding 10 ] [] -- some vertical space
            , row[width fill] [
                  row [alignLeft]
                    [ undo_last_move_button
                    , show_rules_button
                    ]
                , button_game_over
                ]
            , show_rules
            ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init_model Player_vs_AI
        , view = view
        , update = update
        }
