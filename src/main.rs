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
    use std::vec::Vec;

    #[derive(Debug, Copy, Clone, Eq, Ord, PartialEq, PartialOrd)]
    pub enum Chess {
        Pawn,
        Drone,
        Queen,
    }

    impl Chess {
        fn point(&self) -> i32 {
            match self {
                Chess::Pawn => 1,
                Chess::Drone => 2,
                Chess::Queen => 3,
            }
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
        player: Player,
        position: Point,
        movement: Movement,
        effect: Effect,
    }

    impl Action {
        pub fn goal(&self) -> Point {
            let (_, dir, cross) = self.movement.value();
            self.position + dir * (cross as i32 + 1)
        }
    }

    #[derive(Debug, Copy, Clone, Eq, Ord, PartialEq, PartialOrd)]
    pub struct OutOfBounds;

    #[derive(Debug, Copy, Clone, Eq, Ord, PartialEq, PartialOrd)]
    pub struct InvalidAction;

    #[derive(Debug, Copy, Clone, Eq, Ord, PartialEq, PartialOrd)]
    pub struct InvalidMovement;

    #[derive(Debug, Copy, Clone, Eq, Ord, PartialEq, PartialOrd)]
    pub enum GameState {
        Turn(Player),
        Win(Player),
    }

    #[derive(Debug, Copy, Clone, Eq, PartialEq)]
    pub struct Playfield {
        board: [[Option<Chess>; 4]; 8],
        scores: [i32; 2],
        state: GameState,
        previous: Option<Action>,
    }

    impl Playfield {
        pub fn init() -> Playfield {
            let p = Some(Chess::Pawn);
            let d = Some(Chess::Drone);
            let q = Some(Chess::Queen);
            let n = Option::<Chess>::None;

            Playfield {
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

        pub fn get_zone(&self, player: &Player) -> &[[Option<Chess>; 4]] {
            match player {
                Player::Player1 => &self.board[0..4],
                Player::Player2 => &self.board[4..8],
            }
        }

        pub fn get_board_positions(&self) -> impl Iterator<Item=Point> {
            (0..8).into_iter().flat_map(|y| (0..4).into_iter().map(move |x| Point(y, x)))
        }

        pub fn get_zone_positions(&self, player: &Player) -> impl Iterator<Item=Point> {
            let range = match player {
                Player::Player1 => (0..4),
                Player::Player2 => (4..8),
            };
            range.into_iter().flat_map(|y| (0..4).into_iter().map(move |x| Point(y, x)))
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

        pub fn try_get_chess(
            &self,
            position: &Point,
        ) -> Result<(Player, Option<Chess>), OutOfBounds> {
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

        pub fn is_in_zone(&self, position: &Point, player: &Player) -> bool {
            // if let Ok((ref player_, _)) = self.try_get_chess(position) { player_ == player } else { false }
            match player {
                Player::Player1 => {
                    0 <= position.0 && position.0 < 4 && 0 <= position.1 && position.1 < 4
                }
                Player::Player2 => {
                    4 <= position.0 && position.0 < 8 && 0 <= position.1 && position.1 < 4
                }
            }
        }

        pub fn is_empty_at(&self, position: &Point) -> bool {
            // if let Ok((_, None)) = self.try_get_chess(position) { player_ == player } else { false }
            if position.0 >= 0 && position.0 < 8 && position.1 >= 0 && position.1 < 4 {
                self.board[position.0 as usize][position.1 as usize] == None
            } else {
                false
            }
        }

        pub fn try_get_effect(
            &self,
            position: &Point,
            movement: &Movement,
        ) -> Result<Effect, InvalidMovement> {
            let (chess, dir, cross) = movement.value();
            match self.try_get_chess(position) {
                Ok((player, Some(chess_))) if chess_ == chess => {
                    let mut goal = *position;
                    for _ in 0..cross {
                        goal = goal + dir;
                        if self.is_empty_at(&goal) {
                            continue;
                        } else {
                            return Err(InvalidMovement);
                        }
                    }
                    goal = goal + dir;
                    match self.try_get_chess(&goal) {
                        Err(_) => Err(InvalidMovement),
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
                                    return Err(InvalidMovement);
                                };
                                let zone = self.get_zone(&player);
                                if zone.iter().flatten().all(|grid| grid != &Some(promoted)) {
                                    Ok(Effect::Promotion(promoted))
                                } else {
                                    Err(InvalidMovement)
                                }
                            }
                        }
                    }
                }
                _ => Err(InvalidMovement),
            }
        }

        pub fn possible_actions_at(&self, position: &Point) -> Vec<Action> {
            let mut vec = Vec::<Action>::new();

            if let Ok((player, Some(chess))) = self.try_get_chess(position) {
                for movement in Movement::possible_movements_of(&chess) {
                    if let Ok(effect) = self.try_get_effect(position, &movement) {
                        vec.push(Action {
                            player,
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
                && self.is_in_zone(&action.position, &action.player)
                && self.try_get_effect(&action.position, &action.movement) == Ok(action.effect)
        }

        fn unsafe_is_valid_action(&self, action: &Action) -> bool {
            GameState::Turn(action.player) == self.state
                && Some((action.position, action.goal()))
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
            match action.effect {
                Effect::Move => {
                    self.board[start.0 as usize][start.1 as usize] = None;
                    self.board[goal.0 as usize][goal.1 as usize] = Some(chess);
                }
                Effect::Capture(point) => {
                    self.board[start.0 as usize][start.1 as usize] = None;
                    self.board[goal.0 as usize][goal.1 as usize] = Some(chess);
                    match action.player {
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

            let is_empty = self
                .get_zone(&action.player)
                .iter()
                .flatten()
                .all(Option::is_none);
            self.state = match (is_empty, self.scores[0].cmp(&self.scores[1])) {
                (true, Ordering::Greater) => GameState::Win(Player::Player1),
                (true, Ordering::Less) => GameState::Win(Player::Player2),
                (true, Ordering::Equal) => GameState::Win(action.player),
                (false, _) => match action.player {
                    Player::Player1 => GameState::Turn(Player::Player2),
                    Player::Player2 => GameState::Turn(Player::Player1),
                },
            };
        }
    }

    impl Decidable for Playfield {
        type Action = (Point, Action);
        type Score = (bool, i32);

        fn valid_actions(&self) -> Vec<Self::Action> {
            let mut res = Vec::new();
            match self.state {
                GameState::Turn(player) => {
                    for point in self.get_zone_positions(&player) {
                        res.append(&mut self.possible_actions_at(&point))
                    }
                }
                GameState::Win(_) => {}
            }
            res.into_iter()
                .filter(|action| self.unsafe_is_valid_action(action))
                .map(|action| (action.position, action))
                .collect()
        }

        fn apply_action(&self, action: &Self::Action) -> Self {
            let mut state = *self;
            state.unsafe_apply_action(&action.1);
            state
        }

        fn score(&self) -> Self::Score {
            (
                self.state == GameState::Win(Player::Player1),
                self.scores[0] - self.scores[1],
            )
        }
    }
}

mod tui {
    extern crate pancurses;
    use super::algo;
    use super::model;
    use super::utils::Point;
    use pancurses::{endwin, initscr, noecho, Input, Window};

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

    pub struct Board<'a> {
        playfield: &'a mut model::Playfield,
        cursor: Point,
        state: ControlState,
        mode: GameMode,
    }

    impl<'a> Board<'a> {
        pub fn init(playfield: &'a mut model::Playfield, mode: GameMode) -> Self {
            let mut res = Board {
                playfield,
                cursor: Point(0, 0),
                state: ControlState::Pick,
                mode,
            };
            if let &model::GameState::Turn(_) = res.playfield.get_state() {
                res.bot_update();
            }
            res
        }

        fn bot_update(&mut self) {
            match (self.playfield.get_state(), self.mode) {
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
            let is_first =
                self.playfield.get_state() == &model::GameState::Turn(model::Player::Player1);
            self.state = match algo::decide(self.playfield, level, is_first) {
                None => ControlState::BotHalt,
                Some((position, action)) => ControlState::BotMove { position, action },
            };
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

        fn get_bracketed(&self) -> Vec<Point> {
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

    pub trait Control {
        fn up(&mut self);
        fn down(&mut self);
        fn left(&mut self);
        fn right(&mut self);
        fn enter(&mut self);
        fn render(&mut self, window: &Window);
    }

    pub fn control<T>(window: &Window, controller: &mut T)
    where
        T: Control,
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

    impl<'a> Control for Board<'a> {
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
            match (&self.state, self.playfield.try_get_chess(&self.cursor)) {
                (_, Err(_)) | (ControlState::Pick, Ok((_, None))) | (ControlState::BotHalt, _) => {}
                (ControlState::Pick, Ok((player, Some(_))))
                    if &model::GameState::Turn(player) != self.playfield.get_state() => {}
                (ControlState::Pick, Ok((_, Some(_)))) => {
                    self.state = ControlState::Move {
                        position: self.cursor,
                        actions: self
                            .playfield
                            .possible_actions_at(&self.cursor)
                            .into_iter()
                            .filter(|action| self.playfield.is_valid_action(action))
                            .collect(),
                    };
                }
                (
                    ControlState::Move {
                        position,
                        actions: _,
                    },
                    _,
                ) if &self.cursor == position => {
                    self.state = ControlState::Pick;
                }
                (
                    ControlState::Move {
                        position: _,
                        actions,
                    },
                    _,
                ) => {
                    if let Some(action) = actions.iter().find(|action| action.goal() == self.cursor)
                    {
                        if let Ok(()) = self.playfield.try_apply_action(action) {
                            self.state = ControlState::Pick;
                            self.bot_update();
                        }
                    }
                }
                (
                    ControlState::BotMove {
                        position: _,
                        action,
                    },
                    _,
                ) => {
                    if let Ok(()) = self.playfield.try_apply_action(action) {
                        self.state = ControlState::Pick;
                        self.bot_update();
                    }
                }
            }
        }

        fn render(&mut self, window: &Window) {
            let offset: &'a _ = &(2, 2);

            window.clear();
            let highlighted = self.get_highlighted();
            let bracketed = self.get_bracketed();
            let board = self.playfield.get_board();
            let state = self.playfield.get_state();
            let score1 = self.playfield.get_score(&model::Player::Player1);
            let score2 = self.playfield.get_score(&model::Player::Player2);

            for pos @ Point(y, x) in self.playfield.get_board_positions() {
                let sym = match board[y as usize][x as usize] {
                    Some(model::Chess::Pawn) => "*",
                    Some(model::Chess::Drone) => "o",
                    Some(model::Chess::Queen) => "@",
                    None => ".",
                };
                if highlighted.contains(&pos) {
                    window.attron(pancurses::A_BOLD);
                }
                if bracketed.contains(&pos) {
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
    use rand::seq::SliceRandom;
    use std::cmp::Ordering;

    trait TreeLike<L>: Sized {
        fn next(&self) -> Result<Vec<Self>, L>;
    }

    #[derive(Copy, Clone, Ord, Eq, PartialOrd, PartialEq)]
    enum ExtendedOrd<R> {
        Smallest,
        Normal(R),
        Largest,
    }

    fn minimax<T, R>(
        node: &T,
        maximize: bool,
        mut alpha: ExtendedOrd<R>,
        mut beta: ExtendedOrd<R>,
        is_root: bool,
    ) -> ExtendedOrd<R>
    where
        T: TreeLike<R>,
        R: Ord + Copy,
    {
        if maximize {
            match node.next() {
                Ok(subnodes) => {
                    let mut value = ExtendedOrd::Smallest;
                    for subnode in subnodes {
                        if !is_root && value == beta {
                            return beta;
                        }
                        if value > beta {
                            return ExtendedOrd::Largest;
                        }
                        if value > alpha {
                            alpha = value;
                        }
                        let subvalue = minimax(&subnode, !maximize, alpha, beta, false);
                        if subvalue > value {
                            value = subvalue;
                        }
                    }
                    value
                }
                Err(res) => ExtendedOrd::Normal(res),
            }
        } else {
            match node.next() {
                Ok(subnodes) => {
                    let mut value = ExtendedOrd::Largest;
                    for subnode in subnodes {
                        if !is_root && value == alpha {
                            return alpha;
                        }
                        if value < alpha {
                            return ExtendedOrd::Smallest;
                        }
                        if value < beta {
                            beta = value;
                        }
                        let subvalue = minimax(&subnode, !maximize, alpha, beta, false);
                        if subvalue < value {
                            value = subvalue;
                        }
                    }
                    value
                }
                Err(res) => ExtendedOrd::Normal(res),
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
            let actions = state.valid_actions();
            actions
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

    pub fn decide<T>(state: &T, depth: u32, maximize: bool) -> Option<T::Action>
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
                let value_ = minimax(&subnode, !maximize, value, ExtendedOrd::Largest, true);
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
                let value_ = minimax(&subnode, !maximize, ExtendedOrd::Smallest, value, true);
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
        if let ExtendedOrd::Smallest | ExtendedOrd::Largest = value {
            return None;
        }
        let mut rng = rand::thread_rng();
        decisions.shuffle(&mut rng);
        decisions.pop()
    }
}

use std::io::Write;

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
        match (std::io::stdin().read_line(&mut buf), buf.as_str()) {
            (Ok(_), "0\n" | "1\n" | "2\n" | "3\n" | "4\n") => {
                break if buf == "1\n" {
                    tui::GameMode::TwoHumans
                } else if buf == "2\n" {
                    tui::GameMode::HumanBot(3)
                } else if buf == "3\n" {
                    tui::GameMode::BotHuman(3)
                } else if buf == "4\n" {
                    tui::GameMode::TwoBots(4, 4)
                } else {
                    return;
                };
            }
            _ => {
                println!("please input number 0~4");
                buf.clear();
            }
        }
    };

    tui::execute_in_window(|window| {
        let mut playfield = model::Playfield::init();
        let mut board = tui::Board::init(&mut playfield, mode);

        tui::control(window, &mut board);
    })
}
