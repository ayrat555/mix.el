# mix.el

Emacs Minor Mode for Mix, a build tool that ships with Elixir.

## Introduction

I've been using `alchemist` and its built-in functions to work with mix tasks from Emacs: to execute a specific test, to execute tests in a file, to list all tasks etc. They work ok if you don't work with umbrella apps. Umbrella project is an application split into multiple subprojects. `alchemist` uses custom Elixir app under the hood. This Elixir app is initialized only for a specific application. So it's either initialized for a subproject or an umbrella project. Maybe there are customizable variables that allow you to work with umbrella projects and subprojects at the same time but I couldn't find any.

I spend my days writing Elixir code and sometimes I want to execute a Mix task from the root directory for the whole project (for example, to run a credo check) and sometimes I want to execute a task from a subproject (for example, to fetch deps only for this subproject). `mix.el` allows you to do it. Also, it allows you to add additional parameters before executing a mix task.

## Installation

### MELPA

Set up the MELPA or MELPA Stable repository if you haven't already and install with M-x package-install RET mix RET.

### From file

Add `mix.el` to your load path:

``` lisp
(add-to-list 'load-path "path/to/mix.el")
```

## Setup

Add a hook to the mode that you're using with Elixir, for example, `elixir-mode`:

``` lisp
(add-hook 'elixir-mode-hook 'mix-minor-mode)
```

Set `compilation-scroll-output` to non-nil to scroll the *mix* buffer window as output appears. The value ‘first-error’ stops scrolling at the first error, and leaves point on its location in the *mix* buffer. For example:

``` lisp
(setq compilation-scroll-output t)
```

## Usage

`C-c d e` - `mix-execute-task` - List all available tasks and execute one of them. It starts in the root of the umbrella app. As a bonus, you'll get a documentation string because `mix.el` parses shell output of `mix help` directly. Starts in the umbrella root directory.

`C-c d d e` - `mix-execute-task` in an umbrella subproject - The same as `mix-execute-task` but allows you choose a subproject to execute a task in.

`C-c d t` - `mix-test` - Run all test in the app. It starts in the umbrella root directory.

`C-c d d t` - `mix-test` in an umbrella subproject - The same as `mix-test` but allows you to choose a subproject to run tests in.

`C-c d o` - `mix-test-current-buffer` - Run all tests in the current buffer. It starts in the umbrella root directory.

`C-c d d o` - `mix-test-current-buffer` in an umbrella subproject - The same as `mix-test-current-buffer` but runs tests directly from subproject directory.

`C-c d f` - `mix-test-current-test` - Run the current test where pointer is located. It starts in the umbrella root directory.

`C-c d d f` - `mix-test-current-test` in an umbrella subproject - The same as `mix-test-current-test` but runs a test directly from subproject directory.

`C-c d l` - `mix-last-command` - Execute the last mix command.

These are all commands that I use most frequently. You can execute any mix tasks (credo, dialyzer etc) available in the project using `mix-execute-task`. If you have suggestions for additional commands to add keybindings to, please create an issue.

## Prefixes to modify commands before execution

Add these prefixes before commands described in the previous section.

`C-u` - Choose `MIX_ENV` env variable.

`C-u C-u` - Add extra params for mix task.

`C-u C-u C-u` - Choose `MIX_ENV` and add extra params.

For example, to create a migration in a subproject you should press:

`C-u C-u C-c C-c C-c C-e`:
1. `C-u C-u` - to be prompted for migration name
2. `C-c C-c C-c C-e` - to select a mix project and `ecto.gen.migration` task
