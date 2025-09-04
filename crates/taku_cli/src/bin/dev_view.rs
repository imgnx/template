use std::env;
use std::path::{Path, PathBuf};
use std::process::Command;
use winit::application::ApplicationHandler;
use winit::event::WindowEvent;
use winit::event_loop::{ActiveEventLoop, ControlFlow, EventLoop};
use winit::window::{Window, WindowId};
use wry::WebView;
use wry::WebViewBuilder;

#[derive(Default)]
struct App {
    window: Option<Window>,
    webview: Option<WebView>,
    url: String,
}

impl ApplicationHandler for App {
    fn resumed(&mut self, event_loop: &ActiveEventLoop) {
        let window = event_loop
            .create_window(Window::default_attributes())
            .expect("Failed to create window");

        let webview = WebViewBuilder::new()
            .with_url(&self.url)
            .expect("Failed to set URL")
            .build(&window)
            .expect("Failed to build WebView");

        // Optional: open devtools when running against a dev server
        if self.url.starts_with("http://") || self.url.starts_with("https://") {
            webview.open_devtools();
        }

        self.window = Some(window);
        self.webview = Some(webview);
    }

    fn window_event(&mut self, event_loop: &ActiveEventLoop, id: WindowId, event: WindowEvent) {
        match event {
            WindowEvent::CloseRequested => {
                event_loop.exit();
            }
            WindowEvent::RedrawRequested => {
                if let Some(w) = &self.window {
                    w.request_redraw();
                }
            }
            _ => {}
        }
    }
}

fn main() {
    // If an explicit URL is passed, use it. Otherwise run sanity checks.
    let arg_url = env::args().nth(1);
    let is_prod = match env::var("TAKU_ENV") {
        Ok(v) => v.eq_ignore_ascii_case("production") || v == "1" || v.eq_ignore_ascii_case("prod"),
        Err(_) => false,
    } || env::args().any(|a| a == "--prod" || a == "--production");

    let root = env::current_dir().expect("Failed to get current dir");

    let resolved_url = match arg_url {
        Some(u) => u,
        None => {
            if is_prod {
                match find_production_index(&root) {
                    Some(p) => format!("file://{}", p.display()),
                    None => {
                        eprintln!("[taku-cli] ERROR: Production index.html not found in dist/. Run your build first (e.g., `npm run build`).");
                        std::process::exit(2);
                    }
                }
            } else {
                // Development: check for template, trigger bundle, then serve file.
                match find_dev_template(&root) {
                    Some(_tpl) => {
                        if !ensure_bundle(&root) {
                            eprintln!("[taku-cli] ERROR: Bundling failed. Check your webpack/npm logs.");
                            std::process::exit(3);
                        }
                        match find_production_index(&root) {
                            Some(p) => format!("file://{}", p.display()),
                            None => {
                                eprintln!("[taku-cli] ERROR: Bundle did not produce dist/index.html. Ensure webpack is configured to emit into dist/.");
                                std::process::exit(4);
                            }
                        }
                    }
                    None => {
                        if let Some(first) = scan_top_level_html(&root) {
                            eprintln!("[taku-cli] Found top-level HTML at {}. Place templates under `in/` (or src/public/http/www) for consistent builds.", first.display());
                        } else {
                            eprintln!("[taku-cli] No HTML template found. Please add an index.html under `in/` (preferred) or `src/`, `public/`, `http/`, or `www/`.");
                        }
                        // Fall back to dev server if running
                        "http://127.0.0.1:8080".to_string()
                    }
                }
            }
        }
    };

    let event_loop = EventLoop::new().expect("Failed to create event loop");
    event_loop.set_control_flow(ControlFlow::Poll);

    let mut app = App { url: resolved_url, ..Default::default() };
    event_loop.run_app(&mut app);
}

// ---------- Sanity helpers ----------

fn find_production_index(root: &Path) -> Option<PathBuf> {
    let candidates = [
        root.join("antefinem").join("dist").join("index.html"),
        root.join("dist").join("index.html"),
    ];
    candidates.into_iter().find(|p| p.exists())
}

fn find_dev_template(root: &Path) -> Option<PathBuf> {
    let dirs = [
        root.join("antefinem").join("in"),
        root.join("antefinem").join("src"),
        root.join("antefinem").join("public"),
        root.join("antefinem").join("http"),
        root.join("antefinem").join("www"),
        root.join("in"),
        root.join("src"),
        root.join("public"),
        root.join("http"),
        root.join("www"),
    ];
    for d in dirs {
        let p = d.join("index.html");
        if p.exists() { return Some(p); }
    }
    None
}

fn scan_top_level_html(root: &Path) -> Option<PathBuf> {
    if let Ok(read_dir) = std::fs::read_dir(root) {
        for entry in read_dir.flatten() {
            let p = entry.path();
            if p.is_file() {
                if let Some(ext) = p.extension() {
                    if ext == "html" { return Some(p); }
                }
            }
        }
    }
    None
}

fn ensure_bundle(root: &Path) -> bool {
    let target = env::var("BUILD_TARGET").unwrap_or_else(|_| "modular".to_string());
    // Prefer a target-specific build script if present, else fallback to generic build
    let script = format!("build:{}", target);
    let ok = run_npm(root, &script)
        || run_npm(root, "build");
    ok
}

fn run_npm(root: &Path, script: &str) -> bool {
    eprintln!("[taku-cli] Running: npm run {}", script);
    match Command::new("npm").arg("run").arg(script).current_dir(root).status() {
        Ok(status) => status.success(),
        Err(e) => { eprintln!("[taku-cli] Failed to spawn npm: {}", e); false }
    }
}
