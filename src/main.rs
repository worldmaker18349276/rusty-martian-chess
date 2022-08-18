mod utils {
    use std::ops::{Add, Mul};

    #[derive(Debug, Copy, Clone, Eq, Ord, PartialEq, PartialOrd)]
    pub struct Point(pub i32, pub i32);

    impl Add<Point> for Point {
        type Output = Point;

        fn add(self, other: Point) -> Point {
            Point(self.0 + other.0, self.1 + other.1)
        }
    }

    impl Mul<i32> for Point {
        type Output = Point;

        fn mul(self, other: i32) -> Point {
            Point(self.0 * other, self.1 * other)
        }
    }
}

mod model {
    use super::algo::Decidable;
    use super::utils::Point;
    use std::cmp::Ordering;
    use std::fmt::Display;
    use std::vec::Vec;

    #[derive(Debug, Copy, Clone, Eq, Ord, PartialEq, PartialOrd)]
    pub enum Chess {
        Pawn = 1,
        Drone = 2,
        Queen = 3,
    }

    impl Chess {
        fn point(&self) -> i32 {
            *self as i32
        }
    }

    #[derive(Debug, Copy, Clone, Eq, Ord, PartialEq, PartialOrd)]
    pub enum Player {
        Player1,
        Player2,
    }

    #[derive(Debug, Copy, Clone, Eq, Ord, PartialEq, PartialOrd)]
    pub enum Movement {
        PawnUpLeft,
        PawnUpRight,
        PawnDownLeft,
        PawnDownRight,

        DroneUp,
        DroneUp2,
        DroneDown,
        DroneDown2,
        DroneLeft,
        DroneLeft2,
        DroneRight,
        DroneRight2,

        QueenUp(usize),
        QueenDown(usize),
        QueenLeft(usize),
        QueenRight(usize),
        QueenUpLeft(usize),
        QueenUpRight(usize),
        QueenDownLeft(usize),
        QueenDownRight(usize),
    }

    impl Display for Movement {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Movement::PawnUpLeft => write!(f, "Pul"),
                Movement::PawnUpRight => write!(f, "Pur"),
                Movement::PawnDownLeft => write!(f, "Pdl"),
                Movement::PawnDownRight => write!(f, "Pdr"),
                Movement::DroneUp => write!(f, "Du"),
                Movement::DroneUp2 => write!(f, "Du2"),
                Movement::DroneDown => write!(f, "Dd"),
                Movement::DroneDown2 => write!(f, "Dd2"),
                Movement::DroneLeft => write!(f, "Dl"),
                Movement::DroneLeft2 => write!(f, "Dl2"),
                Movement::DroneRight => write!(f, "Dr"),
                Movement::DroneRight2 => write!(f, "Dr2"),
                Movement::QueenUp(0) => write!(f, "Qu"),
                Movement::QueenDown(0) => write!(f, "Qd"),
                Movement::QueenLeft(0) => write!(f, "Ql"),
                Movement::QueenRight(0) => write!(f, "Qr"),
                Movement::QueenUpLeft(0) => write!(f, "Qul"),
                Movement::QueenUpRight(0) => write!(f, "Qur"),
                Movement::QueenDownLeft(0) => write!(f, "Qdl"),
                Movement::QueenDownRight(0) => write!(f, "Qdr"),
                Movement::QueenUp(n) => write!(f, "Qu{}", n + 1),
                Movement::QueenDown(n) => write!(f, "Qd{}", n + 1),
                Movement::QueenLeft(n) => write!(f, "Ql{}", n + 1),
                Movement::QueenRight(n) => write!(f, "Qr{}", n + 1),
                Movement::QueenUpLeft(n) => write!(f, "Qul{}", n + 1),
                Movement::QueenUpRight(n) => write!(f, "Qur{}", n + 1),
                Movement::QueenDownLeft(n) => write!(f, "Qdl{}", n + 1),
                Movement::QueenDownRight(n) => write!(f, "Qdr{}", n + 1),
            }
        }
    }

    pub struct ParseMovementError;

    impl std::str::FromStr for Movement {
        type Err = ParseMovementError;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            match s {
                "Pul" => Ok(Movement::PawnUpLeft),
                "Pur" => Ok(Movement::PawnUpRight),
                "Pdl" => Ok(Movement::PawnDownLeft),
                "Pdr" => Ok(Movement::PawnDownRight),
                "Du" => Ok(Movement::DroneUp),
                "Du2" => Ok(Movement::DroneUp2),
                "Dd" => Ok(Movement::DroneDown),
                "Dd2" => Ok(Movement::DroneDown2),
                "Dl" => Ok(Movement::DroneLeft),
                "Dl2" => Ok(Movement::DroneLeft2),
                "Dr" => Ok(Movement::DroneRight),
                "Dr2" => Ok(Movement::DroneRight2),
                s if s.starts_with('Q') => {
                    fn parse_n(n: &str) -> Result<usize, ParseMovementError> {
                        if n.is_empty() {
                            return Ok(0);
                        }
                        n.parse::<usize>()
                            .map_err(|_| ParseMovementError)
                            .and_then(|n| {
                                if n == 0 {
                                    Err(ParseMovementError)
                                } else {
                                    Ok(n - 1)
                                }
                            })
                    }

                    if let Some(n) = s.strip_prefix("Qul") {
                        parse_n(n).map(Movement::QueenUpLeft)
                    } else if let Some(n) = s.strip_prefix("Qur") {
                        parse_n(n).map(Movement::QueenUpRight)
                    } else if let Some(n) = s.strip_prefix("Qdl") {
                        parse_n(n).map(Movement::QueenDownLeft)
                    } else if let Some(n) = s.strip_prefix("Qdr") {
                        parse_n(n).map(Movement::QueenDownRight)
                    } else if let Some(n) = s.strip_prefix("Qu") {
                        parse_n(n).map(Movement::QueenUp)
                    } else if let Some(n) = s.strip_prefix("Qd") {
                        parse_n(n).map(Movement::QueenDown)
                    } else if let Some(n) = s.strip_prefix("Ql") {
                        parse_n(n).map(Movement::QueenLeft)
                    } else if let Some(n) = s.strip_prefix("Qr") {
                        parse_n(n).map(Movement::QueenRight)
                    } else {
                        Err(ParseMovementError)
                    }
                }
                _ => Err(ParseMovementError),
            }
        }
    }

    impl Movement {
        fn value(&self) -> (Chess, Point, usize) {
            match self {
                Movement::PawnUpLeft => (Chess::Pawn, Point(-1, -1), 0),
                Movement::PawnUpRight => (Chess::Pawn, Point(-1, 1), 0),
                Movement::PawnDownLeft => (Chess::Pawn, Point(1, -1), 0),
                Movement::PawnDownRight => (Chess::Pawn, Point(1, 1), 0),

                Movement::DroneUp => (Chess::Drone, Point(-1, 0), 0),
                Movement::DroneUp2 => (Chess::Drone, Point(-1, 0), 1),
                Movement::DroneDown => (Chess::Drone, Point(1, 0), 0),
                Movement::DroneDown2 => (Chess::Drone, Point(1, 0), 1),
                Movement::DroneLeft => (Chess::Drone, Point(0, -1), 0),
                Movement::DroneLeft2 => (Chess::Drone, Point(0, -1), 1),
                Movement::DroneRight => (Chess::Drone, Point(0, 1), 0),
                Movement::DroneRight2 => (Chess::Drone, Point(0, 1), 1),

                Movement::QueenUp(cross) => (Chess::Queen, Point(-1, 0), *cross),
                Movement::QueenDown(cross) => (Chess::Queen, Point(1, 0), *cross),
                Movement::QueenLeft(cross) => (Chess::Queen, Point(0, -1), *cross),
                Movement::QueenRight(cross) => (Chess::Queen, Point(0, 1), *cross),
                Movement::QueenUpLeft(cross) => (Chess::Queen, Point(-1, -1), *cross),
                Movement::QueenUpRight(cross) => (Chess::Queen, Point(-1, 1), *cross),
                Movement::QueenDownLeft(cross) => (Chess::Queen, Point(1, -1), *cross),
                Movement::QueenDownRight(cross) => (Chess::Queen, Point(1, 1), *cross),
            }
        }

        fn possible_movements_of(chess: &Chess) -> Vec<Self> {
            match chess {
                Chess::Pawn => vec![
                    Movement::PawnUpLeft,
                    Movement::PawnUpRight,
                    Movement::PawnDownLeft,
                    Movement::PawnDownRight,
                ],
                Chess::Drone => vec![
                    Movement::DroneUp,
                    Movement::DroneUp2,
                    Movement::DroneDown,
                    Movement::DroneDown2,
                    Movement::DroneLeft,
                    Movement::DroneLeft2,
                    Movement::DroneRight,
                    Movement::DroneRight2,
                ],
                Chess::Queen => {
                    let mut res = Vec::new();
                    for cross in 0..8 {
                        res.push(Movement::QueenUp(cross));
                        res.push(Movement::QueenDown(cross));
                        res.push(Movement::QueenLeft(cross));
                        res.push(Movement::QueenRight(cross));
                        res.push(Movement::QueenUpLeft(cross));
                        res.push(Movement::QueenUpRight(cross));
                        res.push(Movement::QueenDownLeft(cross));
                        res.push(Movement::QueenDownRight(cross));
                    }
                    res
                }
            }
        }
    }

    #[derive(Debug, Copy, Clone, Eq, Ord, PartialEq, PartialOrd)]
    pub enum Effect {
        Move,
        Capture(i32),
        Promotion(Chess),
    }

    #[derive(Debug, Copy, Clone, Eq, Ord, PartialEq, PartialOrd)]
    pub struct Action {
        position: Point,
        movement: Movement,
        effect: Effect,
    }

    impl Action {
        pub fn position(&self) -> Point {
            self.position
        }

        pub fn goal(&self) -> Point {
            let (_, dir, cross) = self.movement.value();
            self.position + dir * (cross as i32 + 1)
        }
    }

    impl Display for Action {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match &self.effect {
                Effect::Move => {
                    write!(f, "{}{}{}", self.position.0, self.position.1, self.movement)
                }
                Effect::Capture(p) => write!(
                    f,
                    "{}{}{}x{}",
                    self.position.0, self.position.1, self.movement, p
                ),
                Effect::Promotion(Chess::Pawn) => write!(
                    f,
                    "{}{}{}=P",
                    self.position.0, self.position.1, self.movement
                ),
                Effect::Promotion(Chess::Drone) => write!(
                    f,
                    "{}{}{}=D",
                    self.position.0, self.position.1, self.movement
                ),
                Effect::Promotion(Chess::Queen) => write!(
                    f,
                    "{}{}{}=Q",
                    self.position.0, self.position.1, self.movement
                ),
            }
        }
    }

    pub struct ParseActionError;

    impl std::str::FromStr for Action {
        type Err = ParseActionError;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let mut s = s.trim();
            if s.len() < 2 {
                return Err(ParseActionError);
            }

            let position = if let (Ok(x), Ok(y)) = (s[0..1].parse::<i32>(), s[1..2].parse::<i32>())
            {
                s = &s[2..];
                Point(x, y)
            } else {
                return Err(ParseActionError);
            };

            let effect = match &s[s.len() - 2..s.len() - 1] {
                "x" => {
                    let point = if let Ok(point) = s[s.len() - 1..].parse::<i32>() {
                        point
                    } else {
                        return Err(ParseActionError);
                    };
                    s = &s[..s.len() - 2];
                    Effect::Capture(point)
                }
                "=" => {
                    let chess = match &s[s.len() - 1..] {
                        "P" => Chess::Pawn,
                        "D" => Chess::Drone,
                        "Q" => Chess::Queen,
                        _ => {
                            return Err(ParseActionError);
                        }
                    };
                    s = &s[..s.len() - 2];
                    Effect::Promotion(chess)
                }
                _ => Effect::Move,
            };

            let movement = if let Ok(movement) = s.parse::<Movement>() {
                movement
            } else {
                return Err(ParseActionError);
            };

            Ok(Action {
                position,
                movement,
                effect,
            })
        }
    }

    #[derive(Debug, Copy, Clone, Eq, Ord, PartialEq, PartialOrd)]
    pub struct OutOfBounds;

    #[derive(Debug, Copy, Clone, Eq, Ord, PartialEq, PartialOrd)]
    pub struct InvalidAction;

    #[derive(Debug, Copy, Clone, Eq, Ord, PartialEq, PartialOrd)]
    pub enum GameState {
        Turn(Player),
        Win(Player),
    }

    #[derive(Debug, Copy, Clone, Eq, PartialEq)]
    pub struct Game {
        board: [[Option<Chess>; 4]; 8],
        scores: [i32; 2],
        state: GameState,
        previous: Option<Action>,
    }

    impl Display for Game {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "  0 1 2 3")?;
            writeln!(f)?;
            for y in 0..8 {
                write!(f, "{} ", y)?;
                for x in 0..4 {
                    match &self.board[y][x] {
                        None => write!(f, ". ")?,
                        Some(Chess::Pawn) => write!(f, "* ")?,
                        Some(Chess::Drone) => write!(f, "o ")?,
                        Some(Chess::Queen) => write!(f, "@ ")?,
                    }
                }
                writeln!(f)?;
            }
            write!(f, "[{}:{}]", self.scores[0], self.scores[1])
        }
    }

    impl Game {
        pub fn init() -> Game {
            let p = Some(Chess::Pawn);
            let d = Some(Chess::Drone);
            let q = Some(Chess::Queen);
            let n = None;

            Game {
                board: [
                    [q, q, d, n],
                    [q, d, p, n],
                    [d, p, p, n],
                    [n, n, n, n],
                    [n, n, n, n],
                    [n, p, p, d],
                    [n, p, d, q],
                    [n, d, q, q],
                ],
                scores: [0, 0],
                state: GameState::Turn(Player::Player1),
                previous: None,
            }
        }

        pub fn get_board(&self) -> &[[Option<Chess>; 4]; 8] {
            &self.board
        }

        fn get_zone(&self, player: &Player) -> &[[Option<Chess>; 4]] {
            match player {
                Player::Player1 => &self.board[0..4],
                Player::Player2 => &self.board[4..8],
            }
        }

        pub fn get_board_positions() -> impl Iterator<Item = Point> {
            (0..8)
                .into_iter()
                .flat_map(|y| (0..4).into_iter().map(move |x| Point(y, x)))
        }

        fn get_zone_positions(player: &Player) -> impl Iterator<Item = Point> {
            let range = match player {
                Player::Player1 => (0..4),
                Player::Player2 => (4..8),
            };
            range
                .into_iter()
                .flat_map(|y| (0..4).into_iter().map(move |x| Point(y, x)))
        }

        pub fn get_score(&self, player: &Player) -> &i32 {
            match player {
                Player::Player1 => &self.scores[0],
                Player::Player2 => &self.scores[1],
            }
        }

        pub fn get_state(&self) -> &GameState {
            &self.state
        }

        fn try_get_chess(&self, position: &Point) -> Result<(Player, Option<Chess>), OutOfBounds> {
            if position.0 >= 0 && position.0 < 4 && position.1 >= 0 && position.1 < 4 {
                Ok((
                    Player::Player1,
                    self.board[position.0 as usize][position.1 as usize],
                ))
            } else if position.0 >= 4 && position.0 < 8 && position.1 >= 0 && position.1 < 4 {
                Ok((
                    Player::Player2,
                    self.board[position.0 as usize][position.1 as usize],
                ))
            } else {
                Err(OutOfBounds)
            }
        }

        fn try_get_owner(position: &Point) -> Result<Player, OutOfBounds> {
            if position.0 >= 0 && position.0 < 4 && position.1 >= 0 && position.1 < 4 {
                Ok(Player::Player1)
            } else if position.0 >= 4 && position.0 < 8 && position.1 >= 0 && position.1 < 4 {
                Ok(Player::Player2)
            } else {
                Err(OutOfBounds)
            }
        }

        fn is_empty_at(&self, position: &Point) -> bool {
            // if let Ok((_, None)) = self.try_get_chess(position) { player_ == player } else { false }
            if position.0 >= 0 && position.0 < 8 && position.1 >= 0 && position.1 < 4 {
                self.board[position.0 as usize][position.1 as usize] == None
            } else {
                false
            }
        }

        fn try_get_effect(
            &self,
            position: &Point,
            movement: &Movement,
        ) -> Result<Effect, InvalidAction> {
            let (chess, dir, cross) = movement.value();
            match self.try_get_chess(position) {
                Ok((player, Some(chess_))) if chess_ == chess => {
                    let mut goal = *position;
                    for _ in 0..cross {
                        goal = goal + dir;
                        if self.is_empty_at(&goal) {
                            continue;
                        } else {
                            return Err(InvalidAction);
                        }
                    }
                    goal = goal + dir;
                    match self.try_get_chess(&goal) {
                        Err(_) => Err(InvalidAction),
                        Ok((_, None)) => Ok(Effect::Move),
                        Ok((owner, Some(captured))) => {
                            if owner != player {
                                Ok(Effect::Capture(captured.point()))
                            } else {
                                let promoted = if chess == Chess::Pawn && captured == Chess::Pawn {
                                    Chess::Drone
                                } else if chess == Chess::Pawn && captured == Chess::Drone
                                    || chess == Chess::Drone && captured == Chess::Pawn
                                {
                                    Chess::Queen
                                } else {
                                    return Err(InvalidAction);
                                };
                                let zone = self.get_zone(&player);
                                if zone.iter().flatten().all(|grid| grid != &Some(promoted)) {
                                    Ok(Effect::Promotion(promoted))
                                } else {
                                    Err(InvalidAction)
                                }
                            }
                        }
                    }
                }
                _ => Err(InvalidAction),
            }
        }

        pub fn possible_actions_at(&self, position: &Point) -> Vec<Action> {
            let mut vec = Vec::<Action>::new();

            if let Ok((_, Some(chess))) = self.try_get_chess(position) {
                for movement in Movement::possible_movements_of(&chess) {
                    if let Ok(effect) = self.try_get_effect(position, &movement) {
                        vec.push(Action {
                            position: *position,
                            movement,
                            effect,
                        });
                    }
                }
            }
            vec
        }

        pub fn is_valid_action(&self, action: &Action) -> bool {
            self.unsafe_is_valid_action(action)
                && Game::try_get_owner(&action.position).map(GameState::Turn) == Ok(self.state)
                && self.try_get_effect(&action.position, &action.movement) == Ok(action.effect)
        }

        fn unsafe_is_valid_action(&self, action: &Action) -> bool {
            Some((action.position, action.goal()))
                != self.previous.map(|prev| (prev.goal(), prev.position))
        }

        pub fn try_apply_action(&mut self, action: &Action) -> Result<(), InvalidAction> {
            if self.is_valid_action(action) {
                self.unsafe_apply_action(action);
                Ok(())
            } else {
                Err(InvalidAction)
            }
        }

        fn unsafe_apply_action(&mut self, action: &Action) {
            let start = action.position;
            let goal = action.goal();
            let chess = action.movement.value().0;

            let player = if let &GameState::Turn(player) = self.get_state() {
                player
            } else {
                panic!();
            };

            match action.effect {
                Effect::Move => {
                    self.board[start.0 as usize][start.1 as usize] = None;
                    self.board[goal.0 as usize][goal.1 as usize] = Some(chess);
                }
                Effect::Capture(point) => {
                    self.board[start.0 as usize][start.1 as usize] = None;
                    self.board[goal.0 as usize][goal.1 as usize] = Some(chess);
                    match player {
                        Player::Player1 => self.scores[0] += point,
                        Player::Player2 => self.scores[1] += point,
                    }
                }
                Effect::Promotion(promoted) => {
                    self.board[start.0 as usize][start.1 as usize] = None;
                    self.board[goal.0 as usize][goal.1 as usize] = Some(promoted);
                }
            }
            self.previous = Some(*action);

            let is_empty = self.get_zone(&player).iter().flatten().all(Option::is_none);
            self.state = match (is_empty, self.scores[0].cmp(&self.scores[1])) {
                (true, Ordering::Greater) => GameState::Win(Player::Player1),
                (true, Ordering::Less) => GameState::Win(Player::Player2),
                (true, Ordering::Equal) => GameState::Win(player),
                (false, _) => match player {
                    Player::Player1 => GameState::Turn(Player::Player2),
                    Player::Player2 => GameState::Turn(Player::Player1),
                },
            };
        }
    }

    #[derive(Copy, Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
    pub enum Advantage {
        Player2Win(i32),
        StillGaming(i32),
        Player1Win(i32),
    }

    impl Display for Advantage {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{:?}", self)
        }
    }

    impl Decidable for Game {
        type Action = Action;
        type Score = Advantage;

        fn valid_actions(&self) -> Vec<Self::Action> {
            let mut res = Vec::new();
            match self.state {
                GameState::Turn(player) => {
                    for point in Game::get_zone_positions(&player) {
                        res.append(&mut self.possible_actions_at(&point))
                    }
                }
                GameState::Win(_) => {}
            }
            res.into_iter()
                .filter(|action| self.unsafe_is_valid_action(action))
                .collect()
        }

        fn apply_action(&self, action: &Self::Action) -> Self {
            let mut state = *self;
            state.unsafe_apply_action(action);
            state
        }

        fn score(&self) -> Self::Score {
            let adv = self.scores[0] - self.scores[1];
            match self.state {
                GameState::Win(Player::Player1) => Advantage::Player1Win(adv),
                GameState::Win(Player::Player2) => Advantage::Player2Win(adv),
                _ => Advantage::StillGaming(adv),
            }
        }
    }
}

mod playfield {
    use super::algo;
    use super::model;
    use super::tui;
    use super::utils::Point;
    extern crate pancurses;
    use pancurses::Window;

    enum ControlState {
        Pick,
        Move {
            position: Point,
            actions: Vec<model::Action>,
        },
        BotMove {
            position: Point,
            action: model::Action,
        },
        BotHalt,
    }

    #[derive(Debug, Copy, Clone, Eq, Ord, PartialEq, PartialOrd)]
    pub enum GameMode {
        TwoHumans,
        HumanBot(u32),
        BotHuman(u32),
        TwoBots(u32, u32),
    }

    pub struct Playfield<'a> {
        game: &'a mut model::Game,
        cursor: Point,
        state: ControlState,
        mode: GameMode,
    }

    impl<'a> Playfield<'a> {
        pub fn init(game: &'a mut model::Game, mode: GameMode) -> Self {
            let mut res = Playfield {
                game,
                cursor: Point(0, 0),
                state: ControlState::Pick,
                mode,
            };
            if let &model::GameState::Turn(_) = res.game.get_state() {
                res.bot_update();
            }
            res
        }

        fn bot_update(&mut self) {
            match (self.game.get_state(), self.mode) {
                (model::GameState::Win(_), _)
                | (
                    model::GameState::Turn(model::Player::Player1),
                    GameMode::TwoHumans | GameMode::HumanBot(_),
                )
                | (
                    model::GameState::Turn(model::Player::Player2),
                    GameMode::TwoHumans | GameMode::BotHuman(_),
                ) => {}
                (
                    model::GameState::Turn(model::Player::Player1),
                    GameMode::BotHuman(level) | GameMode::TwoBots(level, _),
                )
                | (
                    model::GameState::Turn(model::Player::Player2),
                    GameMode::HumanBot(level) | GameMode::TwoBots(_, level),
                ) => self.bot_move(level),
            }
        }

        fn bot_move(&mut self, level: u32) {
            let is_first = self.game.get_state() == &model::GameState::Turn(model::Player::Player1);
            self.state = match algo::decide(self.game, level, is_first) {
                None => ControlState::BotHalt,
                Some(action) => ControlState::BotMove {
                    position: action.position(),
                    action,
                },
            }
        }

        fn get_highlighted(&self) -> Vec<Point> {
            match &self.state {
                ControlState::Pick | ControlState::BotHalt => vec![],
                ControlState::Move {
                    position,
                    actions: _,
                } => vec![*position],
                ControlState::BotMove {
                    position,
                    action: _,
                } => vec![*position],
            }
        }

        fn get_marked(&self) -> Vec<Point> {
            match &self.state {
                ControlState::Pick | ControlState::BotHalt => vec![],
                ControlState::Move {
                    position: _,
                    actions,
                } => actions.iter().map(|action| action.goal()).collect(),
                ControlState::BotMove {
                    position: _,
                    action,
                } => vec![action.goal()],
            }
        }
    }

    impl<'a> tui::Controllable for Playfield<'a> {
        fn up(&mut self) {
            if self.cursor.0 > 0 {
                self.cursor = self.cursor + Point(-1, 0);
            }
        }

        fn down(&mut self) {
            if self.cursor.0 < 7 {
                self.cursor = self.cursor + Point(1, 0);
            }
        }

        fn left(&mut self) {
            if self.cursor.1 > 0 {
                self.cursor = self.cursor + Point(0, -1);
            }
        }

        fn right(&mut self) {
            if self.cursor.1 < 3 {
                self.cursor = self.cursor + Point(0, 1);
            }
        }

        fn enter(&mut self) {
            match &self.state {
                ControlState::BotHalt => {}

                ControlState::Pick => {
                    if self.game.get_board()[self.cursor.0 as usize][self.cursor.1 as usize]
                        .is_some()
                    {
                        self.state = ControlState::Move {
                            position: self.cursor,
                            actions: self
                                .game
                                .possible_actions_at(&self.cursor)
                                .into_iter()
                                .filter(|action| self.game.is_valid_action(action))
                                .collect(),
                        };
                    }
                }

                ControlState::Move { position, actions } => {
                    if &self.cursor == position {
                        self.state = ControlState::Pick;
                    } else if let Some(action) =
                        actions.iter().find(|action| action.goal() == self.cursor)
                    {
                        if self.game.try_apply_action(action).is_ok() {
                            self.state = ControlState::Pick;
                            self.bot_update();
                        }
                    }
                }

                ControlState::BotMove { position: _, action } => {
                    if self.game.try_apply_action(action).is_ok() {
                        self.state = ControlState::Pick;
                        self.bot_update();
                    }
                }
            }
        }
    }

    impl<'a> tui::Renderable for Playfield<'a> {
        fn render(&mut self, window: &Window) {
            let offset: &'a _ = &(2, 2);

            window.clear();
            let highlighted = self.get_highlighted();
            let marked = self.get_marked();
            let board = self.game.get_board();
            let state = self.game.get_state();
            let score1 = self.game.get_score(&model::Player::Player1);
            let score2 = self.game.get_score(&model::Player::Player2);

            for pos @ Point(y, x) in model::Game::get_board_positions() {
                let sym = match board[y as usize][x as usize] {
                    Some(model::Chess::Pawn) => "*",
                    Some(model::Chess::Drone) => "o",
                    Some(model::Chess::Queen) => "@",
                    None => ".",
                };
                if highlighted.contains(&pos) {
                    window.attron(pancurses::A_BOLD);
                }
                if marked.contains(&pos) {
                    window.attron(pancurses::A_UNDERLINE);
                }
                window.mv(y + offset.0, x * 2 + 1 + offset.1);
                window.addstr(sym);
                window.attroff(pancurses::A_BOLD);
                window.attroff(pancurses::A_UNDERLINE);

                if pos == self.cursor {
                    window.mv(y + offset.0, x * 2 + offset.1);
                    window.addstr("[");
                    window.mv(y + offset.0, x * 2 + 2 + offset.1);
                    window.addstr("]");
                }
            }

            let player1 = if let GameMode::BotHuman(_) | GameMode::TwoBots(_, _) = self.mode {
                "  bot"
            } else {
                "human"
            };
            let player2 = if let GameMode::HumanBot(_) | GameMode::TwoBots(_, _) = self.mode {
                "  bot"
            } else {
                "human"
            };

            window.mv(0, 0);
            match self.state {
                ControlState::Pick => {
                    window.addstr("pick");
                }
                ControlState::Move {
                    position: _,
                    actions: _,
                } => {
                    window.addstr("move");
                }
                ControlState::BotHalt => {
                    window.addstr("halt");
                }
                ControlState::BotMove {
                    position: _,
                    action: _,
                } => {
                    window.addstr("bot");
                }
            }

            window.mv(offset.0, 12 + offset.1);
            match state {
                model::GameState::Turn(model::Player::Player1) => {
                    window.addstr(format!("{}: <{}>", player1, score1));
                }
                model::GameState::Turn(model::Player::Player2) => {
                    window.addstr(format!("{}:  {} ", player1, score1));
                }
                model::GameState::Win(model::Player::Player1) => {
                    window.addstr(format!("{}:  {}  win", player1, score1));
                }
                model::GameState::Win(model::Player::Player2) => {
                    window.addstr(format!("{}:  {}  loss", player1, score1));
                }
            }
            window.mv(7 + offset.0, 12 + offset.1);
            match state {
                model::GameState::Turn(model::Player::Player1) => {
                    window.addstr(format!("{}:  {} ", player2, score2));
                }
                model::GameState::Turn(model::Player::Player2) => {
                    window.addstr(format!("{}: <{}>", player2, score2));
                }
                model::GameState::Win(model::Player::Player1) => {
                    window.addstr(format!("{}:  {}  loss", player2, score2));
                }
                model::GameState::Win(model::Player::Player2) => {
                    window.addstr(format!("{}:  {}  win", player2, score2));
                }
            }
        }
    }
}

mod tui {
    extern crate pancurses;
    use pancurses::{endwin, initscr, noecho, Input, Window};

    pub trait Controllable {
        fn up(&mut self);
        fn down(&mut self);
        fn left(&mut self);
        fn right(&mut self);
        fn enter(&mut self);
    }

    pub trait Renderable {
        fn render(&mut self, window: &Window);
    }

    pub fn control<T>(window: &Window, controller: &mut T)
    where
        T: Controllable + Renderable,
    {
        window.refresh();
        window.keypad(true);
        noecho();

        loop {
            controller.render(window);
            match window.getch() {
                Some(Input::Character('q')) => break,
                Some(Input::KeyUp) => controller.up(),
                Some(Input::KeyDown) => controller.down(),
                Some(Input::KeyLeft) => controller.left(),
                Some(Input::KeyRight) => controller.right(),
                Some(Input::Character('\n')) => controller.enter(),
                _ => (),
            }
        }
        endwin();
    }

    pub fn execute_in_window<T>(func: T)
    where
        T: FnOnce(&Window),
    {
        let window = initscr();
        window.refresh();
        window.keypad(true);
        noecho();

        func(&window);

        endwin();
    }
}

mod algo {
    use std::{cmp::Ordering, fmt::Display};

    use rand::Rng;

    trait TreeLike<L>: Sized {
        fn next(&self) -> Result<Vec<Self>, L>;
    }

    #[derive(Copy, Clone, Ord, Eq, PartialOrd, PartialEq)]
    pub enum ExtendedOrd<R> {
        Smallest,
        Normal(R),
        Largest,
    }

    fn minimax<T, R>(
        node: &T,
        maximize: bool,
        mut alpha: ExtendedOrd<R>,
        mut beta: ExtendedOrd<R>,
    ) -> ExtendedOrd<R>
    where
        T: TreeLike<R>,
        R: Ord + Copy,
    {
        match node.next() {
            Err(res) => ExtendedOrd::Normal(res),
            Ok(subnodes) => {
                if maximize {
                    let mut value = ExtendedOrd::Smallest;
                    for subnode in subnodes {
                        if value == ExtendedOrd::Largest || value > beta {
                            return ExtendedOrd::Largest;
                        }
                        if value > alpha {
                            alpha = value;
                        }
                        let subvalue = minimax(&subnode, !maximize, alpha, beta);
                        if subvalue > value {
                            value = subvalue;
                        }
                    }
                    value
                } else {
                    let mut value = ExtendedOrd::Largest;
                    for subnode in subnodes {
                        if value == ExtendedOrd::Smallest || value < alpha {
                            return ExtendedOrd::Smallest;
                        }
                        if value < beta {
                            beta = value;
                        }
                        let subvalue = minimax(&subnode, !maximize, alpha, beta);
                        if subvalue < value {
                            value = subvalue;
                        }
                    }
                    value
                }
            }
        }
    }

    pub trait Decidable {
        type Action;
        type Score;
        fn valid_actions(&self) -> Vec<Self::Action>;
        fn apply_action(&self, action: &Self::Action) -> Self;
        fn score(&self) -> Self::Score;
    }

    #[derive(Clone)]
    struct DecisionTree<T>
    where
        T: Decidable,
        T::Score: Ord,
    {
        state: T,
        action: T::Action,
        depth: u32,
    }

    impl<T> DecisionTree<T>
    where
        T: Decidable,
        T::Score: Ord,
    {
        fn root(state: &T, depth: u32) -> Vec<Self> {
            state.valid_actions()
                .into_iter()
                .map(|action| DecisionTree {
                    state: state.apply_action(&action),
                    action,
                    depth,
                })
                .collect()
        }
    }

    impl<T> TreeLike<T::Score> for DecisionTree<T>
    where
        T: Decidable,
        T::Score: Ord,
    {
        fn next(&self) -> Result<Vec<Self>, T::Score> {
            if self.depth == 0 {
                return Err(self.state.score());
            }
            let actions = self.state.valid_actions();
            if actions.is_empty() {
                Err(self.state.score())
            } else {
                Ok(actions
                    .into_iter()
                    .map(|action| DecisionTree {
                        state: self.state.apply_action(&action),
                        action,
                        depth: self.depth - 1,
                    })
                    .collect())
            }
        }
    }

    fn decide_multilpe<T>(state: &T, depth: u32, maximize: bool) -> Vec<T::Action>
    where
        T: Decidable,
        T::Score: Ord + Copy,
    {
        let mut value;
        let mut decisions: Vec<T::Action> = Vec::new();
        let subnodes = DecisionTree::root(state, depth);
        if maximize {
            value = ExtendedOrd::Smallest;
            for subnode in subnodes {
                let value_ = minimax(&subnode, !maximize, value, ExtendedOrd::Largest);
                debug_assert!(value_ != ExtendedOrd::Largest);
                match value_.cmp(&value) {
                    Ordering::Greater => {
                        value = value_;
                        decisions.clear();
                        decisions.push(subnode.action);
                    }
                    Ordering::Equal => {
                        decisions.push(subnode.action);
                    }
                    Ordering::Less => {}
                }
            }
        } else {
            value = ExtendedOrd::Largest;
            for subnode in subnodes {
                let value_ = minimax(&subnode, !maximize, ExtendedOrd::Smallest, value);
                debug_assert!(value_ != ExtendedOrd::Smallest);
                match value_.cmp(&value) {
                    Ordering::Less => {
                        value = value_;
                        decisions.clear();
                        decisions.push(subnode.action);
                    }
                    Ordering::Equal => {
                        decisions.push(subnode.action);
                    }
                    Ordering::Greater => {}
                }
            }
        }
        match value {
            ExtendedOrd::Smallest | ExtendedOrd::Largest => vec![],
            ExtendedOrd::Normal(_) => decisions,
        }
    }

    pub fn decide<T>(state: &T, depth: u32, maximize: bool) -> Option<T::Action>
    where
        T: Decidable,
        T::Score: Ord + Copy,
    {
        let mut decisions = decide_multilpe(state, depth, maximize);
        let len = decisions.len();
        if len == 0 {
            None
        } else {
            let n = rand::thread_rng().gen::<f64>();
            let n = (n * len as f64) as usize;
            Some(decisions.remove(n))
        }
    }

    fn minimax_path<T, R>(
        nodes: Vec<T>,
        maximize: bool,
        mut alpha: ExtendedOrd<R>,
        mut beta: ExtendedOrd<R>,
    ) -> (ExtendedOrd<R>, Vec<Vec<T>>)
    where
        T: TreeLike<R> + Clone,
        R: Ord + Copy,
    {
        if maximize {
            let mut decisions = Vec::new();
            let mut value = ExtendedOrd::Smallest;
            for node in nodes {
                if value > beta {
                    return (ExtendedOrd::Largest, vec![]);
                }
                if value > alpha {
                    alpha = value;
                }
                let (subvalue, subdecisions) = match node.next() {
                    Ok(subnodes) => minimax_path(subnodes, !maximize, alpha, beta),
                    Err(res) => (ExtendedOrd::Normal(res), vec![vec![]]),
                };
                if subvalue > value {
                    value = subvalue;
                    decisions.clear();
                }
                if subvalue == value {
                    for mut path in subdecisions {
                        path.insert(0, node.clone());
                        decisions.push(path);
                    }
                }
            }
            (value, decisions)
        } else {
            let mut decisions = Vec::new();
            let mut value = ExtendedOrd::Largest;
            for node in nodes {
                if value < alpha {
                    return (ExtendedOrd::Smallest, vec![]);
                }
                if value < beta {
                    beta = value;
                }
                let (subvalue, subdecisions) = match node.next() {
                    Ok(subnodes) => minimax_path(subnodes, !maximize, alpha, beta),
                    Err(res) => (ExtendedOrd::Normal(res), vec![vec![]]),
                };
                if subvalue < value {
                    value = subvalue;
                    decisions.clear();
                }
                if subvalue == value {
                    for mut path in subdecisions {
                        path.insert(0, node.clone());
                        decisions.push(path);
                    }
                }
            }
            (value, decisions)
        }
    }

    fn decide_paths<T>(
        state: &T,
        depth: u32,
        maximize: bool,
    ) -> (ExtendedOrd<T::Score>, Vec<Vec<T::Action>>)
    where
        T: Decidable + Clone,
        T::Action: Clone,
        T::Score: Ord + Copy,
    {
        let subnodes = DecisionTree::root(state, depth);
        let (value, decisions) = minimax_path(
            subnodes,
            maximize,
            ExtendedOrd::Smallest,
            ExtendedOrd::Largest,
        );
        let decisions = decisions
            .into_iter()
            .map(|path| path.into_iter().map(|node| node.action).collect())
            .collect();
        (value, decisions)
    }

    pub fn decide_path<T>(state: &T, depth: u32, maximize: bool) -> Option<Vec<T::Action>>
    where
        T: Decidable + Clone + Display,
        T::Action: Clone + Display,
        T::Score: Ord + Copy + Display,
    {
        let (_, mut decisions) = decide_paths(state, depth, maximize);
        let len = decisions.len();
        if len == 0 {
            None
        } else {
            let n = rand::thread_rng().gen::<f64>();
            let n = (n * len as f64) as usize;
            Some(decisions.remove(n))
        }
    }
}

use std::io::Write;

const TEXT_MODE: bool = false;

fn main() {
    let mut buf = String::new();
    println!("please select game mode");
    println!("1. two humans");
    println!("2. human v.s. bot");
    println!("3. bot v.s. human");
    println!("4. bot v.s. bot");
    println!("0. quit");
    let mode = loop {
        print!(":");
        std::io::stdout().flush().expect("Could not flush stdout");
        buf.clear();
        std::io::stdin().read_line(&mut buf).unwrap();
        match buf.trim() {
            "0" => return,
            "1" => break playfield::GameMode::TwoHumans,
            "2" => break playfield::GameMode::HumanBot(3),
            "3" => break playfield::GameMode::BotHuman(3),
            "4" => break playfield::GameMode::TwoBots(3, 3),
            _ => println!("please input number 0~4"),
        }
    };

    if TEXT_MODE {
        let mut game = model::Game::init();
        loop {
            println!("{}", game);
            match (game.get_state(), mode) {
                (model::GameState::Win(player), _) => {
                    match player {
                        model::Player::Player1 => {
                            println!("player 1 win");
                        }
                        model::Player::Player2 => {
                            println!("player 2 win");
                        }
                    }
                    break;
                }

                (
                    model::GameState::Turn(player @ model::Player::Player1),
                    playfield::GameMode::TwoHumans | playfield::GameMode::HumanBot(_),
                )
                | (
                    model::GameState::Turn(player @ model::Player::Player2),
                    playfield::GameMode::TwoHumans | playfield::GameMode::BotHuman(_),
                ) => {
                    match player {
                        model::Player::Player1 => {
                            println!("player 1 turn");
                        }
                        model::Player::Player2 => {
                            println!("player 2 turn");
                        }
                    }
                    loop {
                        print!(":");
                        std::io::stdout().flush().expect("Could not flush stdout");
                        buf.clear();
                        std::io::stdin().read_line(&mut buf).unwrap();
                        match buf.parse::<model::Action>() {
                            Ok(action) => match game.try_apply_action(&action) {
                                Ok(_) => break,
                                Err(_) => {
                                    println!("invalid action");
                                }
                            },
                            Err(_) => {
                                println!("invalid action");
                            }
                        }
                    }
                }

                (
                    model::GameState::Turn(player @ model::Player::Player1),
                    playfield::GameMode::BotHuman(level) | playfield::GameMode::TwoBots(level, _),
                )
                | (
                    model::GameState::Turn(player @ model::Player::Player2),
                    playfield::GameMode::HumanBot(level) | playfield::GameMode::TwoBots(_, level),
                ) => {
                    let is_first =
                        game.get_state() == &model::GameState::Turn(model::Player::Player1);
                    // match algo::decide_path(&game, level, is_first) {
                    //     None => {
                    //         println!("bot halt");
                    //         return;
                    //     }
                    //     Some(mut path) => {
                    //         match player {
                    //             model::Player::Player1 => {
                    //                 println!("bot 1 turn");
                    //             }
                    //             model::Player::Player2 => {
                    //                 println!("bot 2 turn");
                    //             }
                    //         }
                    //         print!(":");
                    //         for action in path.iter() {
                    //             print!("{} ", action);
                    //         }
                    //         std::io::stdout().flush().expect("Could not flush stdout");
                    //         let action = path.remove(0);
                    //         buf.clear();
                    //         std::io::stdin().read_line(&mut buf).unwrap();
                    //         game.try_apply_action(&action).unwrap();
                    //     }
                    // }
                    match algo::decide(&game, level, is_first) {
                        None => {
                            println!("bot halt");
                            return;
                        }
                        Some(action) => {
                            match player {
                                model::Player::Player1 => {
                                    println!("bot 1 turn");
                                }
                                model::Player::Player2 => {
                                    println!("bot 2 turn");
                                }
                            }
                            print!(":{}", action);
                            std::io::stdout().flush().expect("Could not flush stdout");
                            buf.clear();
                            std::io::stdin().read_line(&mut buf).unwrap();
                            game.try_apply_action(&action).unwrap();
                        }
                    }
                }
            }
        }
    } else {
        tui::execute_in_window(|window| {
            let mut game = model::Game::init();
            let mut playfield = playfield::Playfield::init(&mut game, mode);

            tui::control(window, &mut playfield);
        })
    }
}
