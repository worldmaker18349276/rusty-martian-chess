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

        pub fn is_in_zone(&self, position: &Point, player: &Player) -> bool {
            match player {
                Player::Player1 => {
                    0 <= position.0 && position.0 < 4 && 0 <= position.1 && position.1 < 4
                }
                Player::Player2 => {
                    4 <= position.0 && position.0 < 8 && 0 <= position.1 && position.1 < 4
                }
            }
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

        pub fn get_chess(&self, position: &Point) -> Result<(Player, Option<Chess>), OutOfBounds> {
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

        pub fn get_effect(
            &self,
            position: &Point,
            movement: &Movement,
        ) -> Result<Effect, InvalidMovement> {
            let (chess, dir, cross) = movement.value();
            match self.get_chess(position) {
                Ok((player, Some(chess_))) if chess_ == chess => {
                    let mut goal = *position;
                    for _ in 0..cross {
                        goal = goal + dir;
                        match self.get_chess(&goal) {
                            Ok((_, None)) => continue,
                            _ => return Err(InvalidMovement),
                        }
                    }
                    goal = goal + dir;
                    match self.get_chess(&goal) {
                        Err(_) => Err(InvalidMovement),
                        Ok((_, None)) => Ok(Effect::Move),
                        Ok((owner, Some(captured))) => {
                            if owner != player {
                                return Ok(Effect::Capture(captured.point()));
                            } else {
                                let promoted = if chess == Chess::Pawn && captured == Chess::Pawn {
                                    Chess::Drone
                                } else if chess == Chess::Pawn && captured == Chess::Drone {
                                    Chess::Queen
                                } else if chess == Chess::Drone && captured == Chess::Pawn {
                                    Chess::Queen
                                } else {
                                    return Err(InvalidMovement);
                                };
                                let zone = self.get_zone(&player);
                                if zone.iter().flatten().all(|grid| grid != &Some(promoted)) {
                                    return Ok(Effect::Promotion(promoted));
                                } else {
                                    return Err(InvalidMovement);
                                }
                            }
                        }
                    }
                }
                _ => return Err(InvalidMovement),
            }
        }

        pub fn possible_actions_at(&self, position: &Point) -> Vec<Action> {
            let mut vec = Vec::<Action>::new();

            if let Ok((player, Some(chess))) = self.get_chess(position) {
                for movement in Movement::possible_movements_of(&chess) {
                    if let Ok(effect) = self.get_effect(position, &movement) {
                        vec.push(Action {
                            player,
                            position: *position,
                            movement,
                            effect,
                        });
                    }
                }
            }
            return vec;
        }

        pub fn is_valid_action(&self, action: &Action) -> bool {
            GameState::Turn(action.player) == self.state
                && self.is_in_zone(&action.position, &action.player)
                && self.get_effect(&action.position, &action.movement) == Ok(action.effect)
                && Some((action.position, action.goal()))
                    != self.previous.map(|prev| (prev.goal(), prev.position))
        }

        pub fn valid_actions(&self) -> Vec<Action> {
            let mut res = Vec::new();
            match self.state {
                GameState::Turn(player) => {
                    let rng = match player {
                        Player::Player1 => 0..4,
                        Player::Player2 => 4..8,
                    };
                    for y in rng {
                        for x in 0..4 {
                            res.append(&mut self.possible_actions_at(&Point(y, x)))
                        }
                    }
                }
                GameState::Win(_) => {}
            }
            return res
                .into_iter()
                .filter(|action| self.is_valid_action(action))
                .collect();
        }

        pub fn apply_action(&mut self, action: &Action) -> Result<(), InvalidAction> {
            match self.state {
                GameState::Turn(player) if self.is_valid_action(action) => {
                    let start = action.position;
                    let goal = action.goal();
                    let chess = action.movement.value().0;
                    // assert_eq!(self.board[start.0 as usize][start.1 as usize], chess);
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

                    let is_empty1 = self
                        .get_zone(&Player::Player1)
                        .iter()
                        .flatten()
                        .all(Option::is_none);
                    let is_empty2 = self
                        .get_zone(&Player::Player2)
                        .iter()
                        .flatten()
                        .all(Option::is_none);
                    self.state = match (is_empty1 || is_empty2, self.scores[0].cmp(&self.scores[1]))
                    {
                        (true, Ordering::Greater) => GameState::Win(Player::Player1),
                        (true, Ordering::Less) => GameState::Win(Player::Player2),
                        (true, Ordering::Equal) => GameState::Win(player),
                        (false, _) => match player {
                            Player::Player1 => GameState::Turn(Player::Player2),
                            Player::Player2 => GameState::Turn(Player::Player1),
                        },
                    };
                    Ok(())
                }
                _ => Err(InvalidAction),
            }
        }
    }
}

mod tui {
    extern crate pancurses;
    use super::model;
    use super::utils::Point;
    use pancurses::{endwin, initscr, noecho, Input};

    enum ControlState {
        Pick,
        Move {
            position: Point,
            actions: Vec<model::Action>,
        },
    }

    pub struct Board<'a> {
        playfield: &'a mut model::Playfield,
        cursor: Point,
        state: ControlState,
    }

    impl<'a> Board<'a> {
        pub fn init(playfield: &'a mut model::Playfield) -> Self {
            Board {
                playfield,
                cursor: Point(0, 0),
                state: ControlState::Pick,
            }
        }

        fn get_highlighted(&self) -> Vec<Point> {
            match &self.state {
                ControlState::Pick => vec![],
                ControlState::Move {
                    position,
                    actions: _,
                } => vec![*position],
            }
        }

        fn get_bracketed(&self) -> Vec<Point> {
            match &self.state {
                ControlState::Pick => vec![],
                ControlState::Move {
                    position: _,
                    actions,
                } => actions.iter().map(|action| action.goal()).collect(),
            }
        }
    }

    pub trait Control {
        fn up(&mut self);
        fn down(&mut self);
        fn left(&mut self);
        fn right(&mut self);
        fn enter(&mut self);
        fn render(&mut self, window: &pancurses::Window);
    }

    pub fn control<T>(window: &pancurses::Window, controller: &mut T)
    where
        T: Control,
    {
        window.printw("Type things, press 'q' to quit\n");
        window.refresh();
        window.keypad(true);
        noecho();

        loop {
            controller.render(&window);
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
            match (&self.state, self.playfield.get_chess(&self.cursor)) {
                (_, Err(_)) => {}
                (ControlState::Pick, Ok((_, None))) => {}
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
                        if let Ok(()) = self.playfield.apply_action(action) {
                            self.state = ControlState::Pick;
                        }
                    }
                }
            }
        }

        fn render(&mut self, window: &pancurses::Window) {
            let offset: &'a _ = &(2, 2);

            window.clear();
            let highlighted = self.get_highlighted();
            let bracketed = self.get_bracketed();
            let board = self.playfield.get_board();
            let state = self.playfield.get_state();
            let score1 = self.playfield.get_score(&model::Player::Player1);
            let score2 = self.playfield.get_score(&model::Player::Player2);

            for y in 0..8 {
                for x in 0..4 {
                    let sym = match board[y][x] {
                        Some(model::Chess::Pawn) => "*",
                        Some(model::Chess::Drone) => "o",
                        Some(model::Chess::Queen) => "@",
                        None => ".",
                    };
                    if highlighted.contains(&Point(y as i32, x as i32)) {
                        window.attron(pancurses::A_BOLD);
                    }
                    if bracketed.contains(&Point(y as i32, x as i32)) {
                        window.attron(pancurses::A_UNDERLINE);
                    }
                    window.mv(y as i32 + offset.0, x as i32 * 2 + 1 + offset.1);
                    window.addstr(sym);
                    window.attroff(pancurses::A_BOLD);
                    window.attroff(pancurses::A_UNDERLINE);

                    if y as i32 == self.cursor.0 && x as i32 == self.cursor.1 {
                        window.mv(y as i32 + offset.0, x as i32 * 2 + offset.1);
                        window.addstr("[");
                        window.mv(y as i32 + offset.0, x as i32 * 2 + 2 + offset.1);
                        window.addstr("]");
                    }
                }
            }
            window.mv(0 + offset.0, 12 + offset.1);
            match state {
                model::GameState::Turn(model::Player::Player1) => {
                    window.addstr(format!("<{}>", score1));
                }
                model::GameState::Turn(model::Player::Player2) => {
                    window.addstr(format!(" {} ", score1));
                }
                model::GameState::Win(model::Player::Player1) => {
                    window.addstr(format!(" {}  win", score1));
                }
                model::GameState::Win(model::Player::Player2) => {
                    window.addstr(format!(" {}  loss", score1));
                }
            }
            window.mv(7 + offset.0, 12 + offset.1);
            match state {
                model::GameState::Turn(model::Player::Player1) => {
                    window.addstr(format!(" {} ", score2));
                }
                model::GameState::Turn(model::Player::Player2) => {
                    window.addstr(format!("<{}>", score2));
                }
                model::GameState::Win(model::Player::Player1) => {
                    window.addstr(format!(" {}  loss", score2));
                }
                model::GameState::Win(model::Player::Player2) => {
                    window.addstr(format!(" {}  win", score2));
                }
            }
        }
    }

    pub fn execute_in_window<T>(func: T)
    where
        T: FnOnce(&pancurses::Window),
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
    pub trait TreeLike<L>
    where
        Self: Sized,
    {
        fn next(&self) -> Result<Vec<Self>, L>;
    }

    fn option_max<T>(a: Option<T>, b: Option<T>) -> Option<T>
    where
        T: Ord
    {
        match (a, b) {
            (None, None) => None,
            (Some(v), None) | (None, Some(v)) => Some(v),
            (Some(v1), Some(v2)) => Some(v1.max(v2)),
        }
    }

    fn option_min<T>(a: Option<T>, b: Option<T>) -> Option<T>
    where
        T: Ord
    {
        match (a, b) {
            (None, None) => None,
            (Some(v), None) | (None, Some(v)) => Some(v),
            (Some(v1), Some(v2)) => Some(v1.min(v2)),
        }
    }

    fn minmax<T, R>(node: T, maximize: bool) -> Option<R>
    where
        T: TreeLike<R>,
        R: Ord,
    {
        if maximize {
            let mut value: Option<R> = None;
            match node.next() {
                Ok(subnodes) => {
                    for subnode in subnodes {
                        value = option_max(value, minmax(subnode, !maximize));
                    }
                },
                Err(res) => value = Some(res),
            };
            value
        } else {
            let mut value: Option<R> = None;
            match node.next() {
                Ok(subnodes) => {
                    for subnode in subnodes {
                        value = option_min(value, minmax(subnode, !maximize));
                    }
                },
                Err(res) => value = Some(res),
            };
            value
        }
    }
}

mod algo {
    pub trait TreeLike<L>
    where
        Self: Sized,
    {
        fn next(&self) -> Result<Vec<Self>, L>;
    }

    fn option_max<T>(a: Option<T>, b: Option<T>) -> Option<T>
    where
        T: Ord
    {
        match (a, b) {
            (None, None) => None,
            (Some(v), None) | (None, Some(v)) => Some(v),
            (Some(v1), Some(v2)) => Some(v1.max(v2)),
        }
    }

    fn option_min<T>(a: Option<T>, b: Option<T>) -> Option<T>
    where
        T: Ord
    {
        match (a, b) {
            (None, None) => None,
            (Some(v), None) | (None, Some(v)) => Some(v),
            (Some(v1), Some(v2)) => Some(v1.min(v2)),
        }
    }

    fn minmax<T, R>(node: T, maximize: bool) -> Option<R>
    where
        T: TreeLike<R>,
        R: Ord,
    {
        if maximize {
            let mut value: Option<R> = None;
            match node.next() {
                Ok(subnodes) => {
                    for subnode in subnodes {
                        value = option_max(value, minmax(subnode, !maximize));
                    }
                },
                Err(res) => value = Some(res),
            };
            value
        } else {
            let mut value: Option<R> = None;
            match node.next() {
                Ok(subnodes) => {
                    for subnode in subnodes {
                        value = option_min(value, minmax(subnode, !maximize));
                    }
                },
                Err(res) => value = Some(res),
            };
            value
        }
    }
}

fn main() {
    tui::execute_in_window(|window| {
        let mut playfield = model::Playfield::init();
        let mut board = tui::Board::init(&mut playfield);

        tui::control(&window, &mut board);
    })
}
