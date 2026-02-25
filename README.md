# dotf v3

Modular dotfile manager with git bare repo + sparse checkout.

## OS Packages

Plugins can declare OS-specific packages under `arch`, `osx`, and `cask` fields. These are installed via the platform's package manager when a plugin is installed.

### Package Categories

| Field  | Manager            | Install command             | Use for                                      |
|--------|--------------------|-----------------------------|----------------------------------------------|
| `arch` | paru (AUR/pacman)  | `paru -S --needed <pkg>`    | Arch Linux / CachyOS packages                |
| `osx`  | Homebrew formula   | `brew install <pkg>`        | macOS CLI tools and formulae                  |
| `cask` | Homebrew cask      | `brew install --cask <pkg>` | macOS GUI apps distributed as `.app` bundles  |

### Homebrew Taps (Third-Party Formulae)

Formulae from third-party Homebrew taps use the full `user/tap/formula` path under `osx`. Brew auto-taps and installs in one step. dotf normalizes the name (extracts the part after the last `/`) when checking installed status.

Pattern: `<github-user>/<tap-repo>/<formula>`

```yaml
plugins:
  shell:
    arch:
      - llmfit
      - models-bin
    osx:
      - llmfit                         # standard formula
      - arimxyer/tap/models            # third-party tap formula
      - gromgit/brewtils/taproom       # third-party tap formula (osx-only)
    cask:
      - alacritty                      # GUI app (.app bundle)
```

### Summary

- **Standard formulae** → `osx` with plain name (e.g. `llmfit`)
- **Third-party tap formulae** → `osx` with full path (e.g. `user/tap/formula`)
- **GUI `.app` bundles** → `cask` with plain name (e.g. `alacritty`)
