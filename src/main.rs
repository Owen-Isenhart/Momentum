use anyhow::Result;
use crossterm::{
    event::{self, DisableMouseCapture, EnableMouseCapture, Event, KeyCode},
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use ratatui::{
    backend::CrosstermBackend,
    layout::{Alignment, Constraint, Direction, Layout, Rect},
    style::{Color, Modifier, Style},
    widgets::{Block, Borders, Cell, Paragraph, Row, Table, Tabs},
    Terminal,
};
use serde::{Deserialize, Serialize};
use std::{
    fs,
    io,
    time::{Duration, Instant},
};

const CONFIG_FILE: &str = "config.json";
const SPLITS_FILE: &str = "splits.json";

// --- DATA STRUCTURES ---

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
enum SerializableKey {
    Char(char),
    Enter,
    Esc,
    Space,
    Backspace,
    Tab,
    F(u8),
    PageUp,
    PageDown,
    Home,
    End,
    Unknown,
}

impl From<KeyCode> for SerializableKey {
    fn from(k: KeyCode) -> Self {
        match k {
            KeyCode::Char(c) => {
                if c == ' ' { SerializableKey::Space } else { SerializableKey::Char(c) }
            },
            KeyCode::Enter => SerializableKey::Enter,
            KeyCode::Esc => SerializableKey::Esc,
            KeyCode::Backspace => SerializableKey::Backspace,
            KeyCode::Tab => SerializableKey::Tab,
            KeyCode::F(n) => SerializableKey::F(n),
            KeyCode::PageUp => SerializableKey::PageUp,
            KeyCode::PageDown => SerializableKey::PageDown,
            KeyCode::Home => SerializableKey::Home,
            KeyCode::End => SerializableKey::End,
            _ => SerializableKey::Unknown,
        }
    }
}

impl From<SerializableKey> for KeyCode {
    fn from(k: SerializableKey) -> Self {
        match k {
            SerializableKey::Char(c) => KeyCode::Char(c),
            SerializableKey::Space => KeyCode::Char(' '),
            SerializableKey::Enter => KeyCode::Enter,
            SerializableKey::Esc => KeyCode::Esc,
            SerializableKey::Backspace => KeyCode::Backspace,
            SerializableKey::Tab => KeyCode::Tab,
            SerializableKey::F(n) => KeyCode::F(n),
            SerializableKey::PageUp => KeyCode::PageUp,
            SerializableKey::PageDown => KeyCode::PageDown,
            SerializableKey::Home => KeyCode::Home,
            SerializableKey::End => KeyCode::End,
            SerializableKey::Unknown => KeyCode::Null,
        }
    }
}

impl std::fmt::Display for SerializableKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SerializableKey::Char(c) => write!(f, "{}", c.to_uppercase()),
            SerializableKey::Space => write!(f, "Space"),
            _ => write!(f, "{:?}", self),
        }
    }
}

#[derive(Serialize, Deserialize, Clone)]
struct KeyBindings {
    split: SerializableKey,
    reset: SerializableKey,
    pause: SerializableKey,
    quit: SerializableKey,
    toggle_view: SerializableKey,
}

impl Default for KeyBindings {
    fn default() -> Self {
        Self {
            split: SerializableKey::Space,
            reset: SerializableKey::Char('r'),
            pause: SerializableKey::Char('p'),
            quit: SerializableKey::Char('q'),
            toggle_view: SerializableKey::Tab,
        }
    }
}

#[derive(Serialize, Deserialize, Clone)]
struct Split {
    name: String,
    pb_time: Option<u64>,
    current_time: Option<u64>,
}

#[derive(Serialize, Deserialize, Clone)]
struct RunData {
    run_name: String,
    splits: Vec<Split>,
}

#[derive(Serialize, Deserialize)]
struct SplitsFile {
    runs: Vec<RunData>,
}

#[derive(PartialEq)]
enum AppView {
    Timer,
    LoadSave,
    Settings,
}

#[derive(PartialEq)]
enum AppMode {
    SelectRun,  // Choose a run before starting
    Timer,      // In-app normal mode
}

struct App {
    // State
    all_runs: Vec<RunData>,
    current_run: Option<RunData>,
    config: KeyBindings,
    view: AppView,
    mode: AppMode,
    
    // Timer Logic
    current_split_index: usize,
    start_time: Option<Instant>,
    elapsed_time: Duration,
    running: bool,
    finished: bool,

    // UI Navigation
    selected_index: usize,
    waiting_for_key: Option<&'static str>,
    status_message: String,
    status_time: Option<Instant>,
}

impl App {
    fn new() -> App {
        let config = App::load_config();
        let all_runs = App::load_all_runs();

        App {
            all_runs,
            current_run: None,
            config,
            view: AppView::Timer,
            mode: AppMode::SelectRun,
            current_split_index: 0,
            start_time: None,
            elapsed_time: Duration::from_secs(0),
            running: false,
            finished: false,
            selected_index: 0,
            waiting_for_key: None,
            status_message: String::new(),
            status_time: None,
        }
    }

    // --- PERSISTENCE ---

    fn load_config() -> KeyBindings {
        if let Ok(content) = fs::read_to_string(CONFIG_FILE) {
            serde_json::from_str(&content).unwrap_or_default()
        } else {
            KeyBindings::default()
        }
    }

    fn save_config(&self) {
        if let Ok(json) = serde_json::to_string_pretty(&self.config) {
            let _ = fs::write(CONFIG_FILE, json);
        }
    }

    fn load_all_runs() -> Vec<RunData> {
        if let Ok(content) = fs::read_to_string(SPLITS_FILE) {
            if let Ok(splits_file) = serde_json::from_str::<SplitsFile>(&content) {
                return splits_file.runs;
            }
        }
        vec![RunData {
            run_name: "Default Run".to_string(),
            splits: App::default_splits(),
        }]
    }

    fn save_all_runs(&self) {
        let splits_file = SplitsFile {
            runs: self.all_runs.clone(),
        };
        if let Ok(json) = serde_json::to_string_pretty(&splits_file) {
            let _ = fs::write(SPLITS_FILE, json);
        }
    }

    fn save_current_run(&mut self) {
        if let Some(ref run) = self.current_run {
            if let Some(pos) = self.all_runs.iter().position(|r| r.run_name == run.run_name) {
                self.all_runs[pos] = run.clone();
            }
            self.save_all_runs();
        }
    }

    fn save_and_update_pbs(&mut self) {
        if let Some(ref mut run) = self.current_run {
            for split in &mut run.splits {
                if let Some(current_time) = split.current_time {
                    split.pb_time = Some(current_time);
                }
            }
            self.save_current_run();
            self.set_status("PBs Updated from current run!");
        }
    }

    fn default_splits() -> Vec<Split> {
        vec![
            Split { name: "Village Start".to_string(), pb_time: Some(65000), current_time: None },
            Split { name: "Forest Temple".to_string(), pb_time: Some(125000), current_time: None },
            Split { name: "Fire Boss".to_string(), pb_time: Some(190000), current_time: None },
            Split { name: "End Game".to_string(), pb_time: Some(250000), current_time: None },
        ]
    }

    fn set_status(&mut self, msg: &str) {
        self.status_message = msg.to_string();
        self.status_time = Some(Instant::now());
    }

    // --- LOGIC ---

    fn toggle_timer(&mut self) {
        if self.finished { return; }
        if self.running {
            self.running = false;
            if let Some(start) = self.start_time {
                self.elapsed_time += start.elapsed();
            }
            self.start_time = None;
        } else {
            self.running = true;
            self.start_time = Some(Instant::now());
        }
    }

    fn split(&mut self) {
        if !self.running && !self.finished {
            self.toggle_timer();
            return;
        }
        if self.finished { return; }
        if self.current_run.is_none() { return; }

        let total_current = self.get_display_time();
        if let Some(ref mut run) = self.current_run {
            run.splits[self.current_split_index].current_time = Some(total_current.as_millis() as u64);

            if self.current_split_index < run.splits.len() - 1 {
                self.current_split_index += 1;
            } else {
                self.finished = true;
                self.running = false;
                if let Some(start) = self.start_time {
                    self.elapsed_time += start.elapsed();
                }
                self.start_time = None;
            }
        }
    }

    fn reset(&mut self) {
        self.running = false;
        self.finished = false;
        self.start_time = None;
        self.elapsed_time = Duration::from_secs(0);
        self.current_split_index = 0;
        if let Some(ref mut run) = self.current_run {
            for split in &mut run.splits {
                split.current_time = None;
            }
        }
    }

    fn get_display_time(&self) -> Duration {
        let current_segment = if let Some(start) = self.start_time {
            start.elapsed()
        } else {
            Duration::from_secs(0)
        };
        self.elapsed_time + current_segment
    }

    // --- SETTINGS INPUT HANDLER ---
    
    fn handle_settings_input(&mut self, key: KeyCode) {
        if let Some(action) = self.waiting_for_key {
            // REBINDING LOGIC
            let new_key = SerializableKey::from(key);
            match action {
                "split" => self.config.split = new_key,
                "reset" => self.config.reset = new_key,
                "pause" => self.config.pause = new_key,
                "quit" => self.config.quit = new_key,
                _ => {}
            }
            self.save_config();
            self.waiting_for_key = None;
            self.set_status("Key Updated!");
            return;
        }

        match key {
            KeyCode::Up => {
                if self.selected_index > 0 { self.selected_index -= 1; }
            }
            KeyCode::Down => {
                if self.selected_index < 4 { self.selected_index += 1; }
            }
            KeyCode::Enter => {
                match self.selected_index {
                    0 => self.waiting_for_key = Some("split"),
                    1 => self.waiting_for_key = Some("pause"),
                    2 => self.waiting_for_key = Some("reset"),
                    3 => self.waiting_for_key = Some("quit"),
                    _ => {}
                }
            }
            _ => {}
        }
    }

    fn handle_loadsave_input(&mut self, key: KeyCode) {
        match key {
            KeyCode::Up => {
                if self.selected_index > 0 { self.selected_index -= 1; }
            }
            KeyCode::Down => {
                let max_index = self.all_runs.len() + 1; // runs + save option
                if self.selected_index < max_index { self.selected_index += 1; }
            }
            KeyCode::Enter => {
                if self.selected_index == self.all_runs.len() {
                    // "Save Current Run & Update PBs" action
                    self.save_and_update_pbs();
                } else if self.selected_index < self.all_runs.len() {
                    // Load selected run
                    self.current_run = Some(self.all_runs[self.selected_index].clone());
                    self.reset();
                    self.view = AppView::Timer;
                    self.set_status(&format!("Loaded: {}", self.all_runs[self.selected_index].run_name));
                }
            }
            _ => {}
        }
    }
}

// --- UTILS ---

fn get_big_text_lines(text: &str) -> Vec<String> {
    let mut rows = vec![String::new(); 5];
    
    for c in text.chars() {
        // 3-wide (plus 1 space) x 5-high patterns
        let patterns: [&str; 5] = match c {
            '0' => ["███ ", "█ █ ", "█ █ ", "█ █ ", "███ "],
            '1' => ["  █ ", "  █ ", "  █ ", "  █ ", "  █ "],
            '2' => ["███ ", "  █ ", "███ ", "█   ", "███ "],
            '3' => ["███ ", "  █ ", "███ ", "  █ ", "███ "],
            '4' => ["█ █ ", "█ █ ", "███ ", "  █ ", "  █ "],
            '5' => ["███ ", "█   ", "███ ", "  █ ", "███ "],
            '6' => ["███ ", "█   ", "███ ", "█ █ ", "███ "],
            '7' => ["███ ", "  █ ", "  █ ", "  █ ", "  █ "],
            '8' => ["███ ", "█ █ ", "███ ", "█ █ ", "███ "],
            '9' => ["███ ", "█ █ ", "███ ", "  █ ", "███ "],
            ':' => ["    ", " █  ", "    ", " █  ", "    "],
            '.' => ["    ", "    ", "    ", "    ", " █  "],
            _   => ["    ", "    ", "    ", "    ", "    "],
        };

        for (i, line) in patterns.iter().enumerate() {
            rows[i].push_str(line);
        }
    }
    rows
}

fn format_duration(d: Duration) -> String {
    let millis = d.as_millis();
    let seconds = millis / 1000;
    let minutes = seconds / 60;
    let rem_seconds = seconds % 60;
    let rem_millis = (millis % 1000) / 10;
    format!("{:02}:{:02}.{:02}", minutes, rem_seconds, rem_millis)
}

fn format_delta(current: u64, pb: u64) -> String {
    let diff = current as i64 - pb as i64;
    let sign = if diff < 0 { "-" } else { "+" };
    let abs_diff = diff.abs() as u64;
    let seconds = abs_diff / 1000;
    let millis = (abs_diff % 1000) / 10;
    format!("{}{}.{:02}", sign, seconds, millis)
}

// --- UI RENDERING ---

fn ui(f: &mut ratatui::Frame, app: &App) {
    if app.mode == AppMode::SelectRun {
        draw_run_selection(f, app);
        return;
    }

    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints(
            [
                Constraint::Length(3), // Tabs
                Constraint::Min(0),    // Content
                Constraint::Length(1), // Status Bar
            ]
            .as_ref(),
        )
        .split(f.size());

    // 1. TABS
    let titles = vec!["(1) Timer", "(2) Load/Save", "(3) Settings"];
    let tab_index = match app.view {
        AppView::Timer => 0,
        AppView::LoadSave => 1,
        AppView::Settings => 2,
    };
    
    let tabs = Tabs::new(titles)
        .block(Block::default().borders(Borders::ALL))
        .select(tab_index)
        .highlight_style(Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD));
    f.render_widget(tabs, chunks[0]);

    // 2. CONTENT
    match app.view {
        AppView::Timer => draw_timer(f, app, chunks[1]),
        AppView::LoadSave => draw_loadsave(f, app, chunks[1]),
        AppView::Settings => draw_settings(f, app, chunks[1]),
    }

    // 3. STATUS BAR
    if let Some(time) = app.status_time {
        if time.elapsed().as_secs() < 3 {
             let status = Paragraph::new(format!("Msg: {}", app.status_message))
                .style(Style::default().fg(Color::Magenta));
            f.render_widget(status, chunks[2]);
        }
    }
}

fn draw_run_selection(f: &mut ratatui::Frame, app: &App) {
    let area = f.size();
    
    let mut items = app.all_runs.iter().map(|r| r.run_name.as_str()).collect::<Vec<_>>();
    items.push("Create New Run");

    let selected_style = Style::default().fg(Color::Black).bg(Color::Cyan);
    let normal_style = Style::default();

    let rows = items.iter().enumerate().map(|(i, name)| {
        let style = if i == app.selected_index { selected_style } else { normal_style };
        Row::new(vec![Cell::from(*name)]).style(style)
    });

    let table = Table::new(rows, [Constraint::Percentage(100)])
        .block(Block::default().borders(Borders::ALL).title("Select a Run"));

    f.render_widget(table, area);
}

fn draw_timer(f: &mut ratatui::Frame, app: &App, area: Rect) {
    if app.current_run.is_none() {
        f.render_widget(
            Paragraph::new("No run selected. Go to Load/Save to select a run."),
            area,
        );
        return;
    }

    let run = app.current_run.as_ref().unwrap();
    
    // CHANGED: Middle constraint increased to Length(7) to fit the big text height
    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints([Constraint::Min(5), Constraint::Length(7), Constraint::Length(3)].as_ref())
        .split(area);

    // --- SPLITS TABLE (Same as before) ---
    let header_cells = ["Segment", "PB Time", "Split Time", "+/-"]
        .iter()
        .map(|h| Cell::from(*h).style(Style::default().fg(Color::Yellow)));
    let header = Row::new(header_cells).height(1).bottom_margin(1);

    let rows = run.splits.iter().enumerate().map(|(i, split)| {
        let is_current = i == app.current_split_index;
        
        let pb_text = match split.pb_time {
            Some(ms) => format_duration(Duration::from_millis(ms)),
            None => "-".to_string(),
        };

        let current_text = match split.current_time {
            Some(ms) => format_duration(Duration::from_millis(ms)),
            None => if is_current && app.running { "..." .to_string() } else { "-".to_string() },
        };

        let delta_text = if let (Some(curr), Some(pb)) = (split.current_time, split.pb_time) {
             format_delta(curr, pb)
        } else {
            String::new()
        };

        let style = if is_current {
            Style::default().fg(Color::Black).bg(Color::White)
        } else {
            Style::default()
        };

        let delta_style = if delta_text.starts_with('-') {
            Style::default().fg(Color::Green)
        } else if delta_text.starts_with('+') {
            Style::default().fg(Color::Red)
        } else {
            style
        };

        Row::new(vec![
            Cell::from(split.name.clone()),
            Cell::from(pb_text),
            Cell::from(current_text),
            Cell::from(delta_text).style(delta_style),
        ]).style(style)
    });

    let t = Table::new(
        rows,
        [Constraint::Percentage(40), Constraint::Percentage(20), Constraint::Percentage(20), Constraint::Percentage(20)]
    )
    .header(header)
    .block(Block::default().borders(Borders::ALL).title(format!("Run: {}", run.run_name)));
    
    f.render_widget(t, chunks[0]);

    // --- MAIN TIMER (Updated for Big Text) ---
    let display_time = app.get_display_time();
    let timer_text = format_duration(display_time);
    let timer_color = if app.finished { Color::Green } else { Color::White };
    
    // Check if we have enough height to render big text (needs 5 lines + 2 borders = 7)
    let timer_area = chunks[1];
    
    let timer_paragraph = if timer_area.height >= 7 {
        let big_lines = get_big_text_lines(&timer_text);
        let joined = big_lines.join("\n");
        Paragraph::new(joined)
            .style(Style::default().fg(timer_color))
            .alignment(Alignment::Center)
            .block(Block::default().borders(Borders::ALL).title("Total Time"))
    } else {
        // Fallback to small text if window is too small
        Paragraph::new(timer_text)
            .style(Style::default().fg(timer_color).add_modifier(Modifier::BOLD))
            .alignment(Alignment::Center)
            .block(Block::default().borders(Borders::ALL).title("Total Time"))
    };

    f.render_widget(timer_paragraph, chunks[1]);
    
    // --- HELP ---
    let help_txt = format!(
        "Controls: {} to Split | {} to Pause | {} to Reset", 
        app.config.split, app.config.pause, app.config.reset
    );
    f.render_widget(Paragraph::new(help_txt).alignment(Alignment::Center).style(Style::default().fg(Color::Gray)), chunks[2]);
}
fn draw_settings(f: &mut ratatui::Frame, app: &App, area: Rect) {
    let settings_items = vec![
        ("Split Key", app.config.split.to_string()),
        ("Pause Key", app.config.pause.to_string()),
        ("Reset Key", app.config.reset.to_string()),
        ("Quit Key",  app.config.quit.to_string()),
    ];

    let rows = settings_items.iter().enumerate().map(|(i, (label, val))| {
        let style = if i == app.selected_index {
            if app.waiting_for_key.is_some() {
                 Style::default().fg(Color::Yellow).add_modifier(Modifier::SLOW_BLINK)
            } else {
                 Style::default().fg(Color::Black).bg(Color::Cyan)
            }
        } else {
            Style::default()
        };

        Row::new(vec![
            Cell::from(*label),
            Cell::from(if app.waiting_for_key.is_some() && i == app.selected_index { "Press any key..." .to_string() } else { val.clone() }),
        ]).style(style)
    });

    let t = Table::new(
        rows,
        [Constraint::Percentage(50), Constraint::Percentage(50)]
    )
    .block(Block::default().borders(Borders::ALL).title("Key Bindings"))
    .highlight_style(Style::default().add_modifier(Modifier::BOLD));

    f.render_widget(t, area);
}

fn draw_loadsave(f: &mut ratatui::Frame, app: &App, area: Rect) {
    let mut items = Vec::new();
    
    for run in &app.all_runs {
        items.push((run.run_name.clone(), "Load".to_string()));
    }
    
    items.push(("Save & Update PBs from Current Run".to_string(), "Action".to_string()));

    let rows = items.iter().enumerate().map(|(i, (label, action))| {
        let style = if i == app.selected_index {
            Style::default().fg(Color::Black).bg(Color::Cyan)
        } else {
            Style::default()
        };

        Row::new(vec![
            Cell::from(label.clone()),
            Cell::from(action.clone()),
        ]).style(style)
    });

    let t = Table::new(
        rows,
        [Constraint::Percentage(70), Constraint::Percentage(30)]
    )
    .block(Block::default().borders(Borders::ALL).title("Load/Save Runs"))
    .highlight_style(Style::default().add_modifier(Modifier::BOLD));

    f.render_widget(t, area);
}

// --- MAIN LOOP ---

fn main() -> Result<()> {
    enable_raw_mode()?;
    let mut stdout = io::stdout();
    execute!(stdout, EnterAlternateScreen, EnableMouseCapture)?;
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;

    let mut app = App::new();
    let tick_rate = Duration::from_millis(30);
    let mut last_tick = Instant::now();

    loop {
        terminal.draw(|f| ui(f, &app))?;

        let timeout = tick_rate
            .checked_sub(last_tick.elapsed())
            .unwrap_or_else(|| Duration::from_secs(0));

        if crossterm::event::poll(timeout)? {
            if let Event::Key(key) = event::read()? {
                let code = key.code;
                
                // GLOBAL QUIT CHECK (unless rebinding)
                if app.waiting_for_key.is_none() && SerializableKey::from(code) == app.config.quit {
                    break;
                }

                // RUN SELECTION MODE
                if app.mode == AppMode::SelectRun {
                    match code {
                        KeyCode::Up => {
                            if app.selected_index > 0 { app.selected_index -= 1; }
                        }
                        KeyCode::Down => {
                            let max = app.all_runs.len();
                            if app.selected_index < max { app.selected_index += 1; }
                        }
                        KeyCode::Enter => {
                            if app.selected_index == app.all_runs.len() {
                                // Create new run
                                let new_run = RunData {
                                    run_name: format!("Run {}", app.all_runs.len() + 1),
                                    splits: App::default_splits(),
                                };
                                app.all_runs.push(new_run.clone());
                                app.current_run = Some(new_run);
                            } else {
                                // Load existing run
                                app.current_run = Some(app.all_runs[app.selected_index].clone());
                            }
                            app.save_all_runs();
                            app.mode = AppMode::Timer;
                            app.selected_index = 0;
                            app.reset();
                        }
                        _ => {}
                    }
                    continue;
                }

                // NUMBER KEY TAB NAVIGATION
                match code {
                    KeyCode::Char('1') if app.waiting_for_key.is_none() => {
                        app.view = AppView::Timer;
                        app.selected_index = 0;
                    }
                    KeyCode::Char('2') if app.waiting_for_key.is_none() => {
                        app.view = AppView::LoadSave;
                        app.selected_index = 0;
                    }
                    KeyCode::Char('3') if app.waiting_for_key.is_none() => {
                        app.view = AppView::Settings;
                        app.selected_index = 0;
                    }
                    _ => {}
                }

                // INPUT ROUTING FOR NORMAL MODE
                match app.view {
                    AppView::Settings => {
                        app.handle_settings_input(code);
                    },
                    AppView::LoadSave => {
                        app.handle_loadsave_input(code);
                    },
                    AppView::Timer => {
                        // Map code to action
                        let k = SerializableKey::from(code);
                        if k == app.config.split { app.split(); }
                        else if k == app.config.reset { app.reset(); }
                        else if k == app.config.pause { app.toggle_timer(); }
                    }
                }
            }
        }

        if last_tick.elapsed() >= tick_rate {
            last_tick = Instant::now();
        }
    }

    disable_raw_mode()?;
    execute!(terminal.backend_mut(), LeaveAlternateScreen, DisableMouseCapture)?;
    terminal.show_cursor()?;

    Ok(())
}