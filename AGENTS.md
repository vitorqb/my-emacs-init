# Agent Guide: emacs-init

Personal Emacs configuration with custom packages and modular structure.

## Architecture

- **Entry Point**: `emacs-init.el`. Loaded via `~/.emacs` using `(load-file "...")`.
- **Modules**: `modules/*.el`. Lightweight, on-demand configurations.
  - Load via `(emacs-init-load-module-<name>)`.
- **Packages**: `packages/*`. Custom local Elisp packages.
  - Automatically added to `load-path` if they are directories.
  - Each usually has a `Makefile` and `Cask` file.
- **Vendor**: `vendor/*`. External libraries added to `load-path`.
- **Stubs**: `stubs/*`. Local executables used by the config (e.g., `copilot-language-server`).

## Key Conventions

- **Package Management**: Uses `use-package` with `:ensure t` (enabled globally).
- **UI/Completion**: `ivy`, `counsel`, `swiper`.
- **Keybindings**: Centered around `Hydra`. Main entry point is `C-.`.
- **Search**: `deadgrep` and `counsel-rg` are preferred.
- **LSP**: `eglot` is the default.
- **Code Style**: Prefers `dash.el` (`-` prefix) and `s.el` (`s-` prefix). Use lexical binding for new files.

## Developer Commands

### Testing Packages
Each custom package in `packages/` should be tested individually:
```bash
cd packages/<package-name>
make test
```
*Note: Requires `cask` to be installed.*

### Configuration
- **User Overrides**: `~/.config/emacs_init/config.el`.
- **Post-init Hook**: `~/.config/emacs_init/hook.el`.
- **Profile Hooks**: `~/.config/emacs_init/profile-hooks/<profile-name>.el`.

## Troubleshooting

- If a module doesn't load: check if it's defined in `emacs-init.el` using `my/defmodule`.
- If a package is not found: ensure it's a directory in `packages/` and doesn't start with `.`.
- **Copilot**: Requires the stub in `stubs/copilot-language-server` to be present and executable.
