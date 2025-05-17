[![Tests](https://github.com/johannes-mueller/devcontainer.el/actions/workflows/test.yml/badge.svg)](https://github.com/johannes-mueller/devcontainer.el/actions/workflows/test.yml)

# devcontainer.el

Rudimentary devcontainer support for Emacs


## Synopsis

This package lets you handle i.e. start, restart devcontainers of your projects
and forwards all your `compile` commands into the devcontainer by advising
`compilation-start`.  So as long you use `compile` to build, test and run
(parts of your) software, all will be executed inside the devcontainer.


## Motivation

### What are devcontainers?

[Devcontainers](https://containers.dev/) become increasingly popular in modern
software development.  The idea is to put everything the software you are
developing needs in order to run into a docker container or a stack of
[docker](https://docker.com) containers.  Then all your local builds and
testing is performed inside that container.  So your development basically
takes place inside the docker container – hence the name "devcontainer".

The initiative came from the Visual Studio Code community. They actually run
the complete tooling inside the container, even their IDE.  The configuration
of the IDE – i.e. which extensions are to be installed – is also defined by the
definition of the devcontainer.


### Approach for support in Emacs

While you technically don't need to use devcontainers even though your team is
using them, it is convenient for you also to use devcontainers at least to some
degree.  Of course, you won't install an Emacs instance inside the
devcontainer, but it is really helpful for you to manage devcontainers from
within Emacs and interact with the installation of your software inside of it.
That's where the `devcontainer` package comes in.

`devcontainer` provides a set Emacs commands to build, launch, stop, erase
and rebuild the devcontainer according to its definition in the project's repo.
In order for you to run and test your software inside the devcontainer,
`devcontainer-mode` advises Emacs' `compilation-start` function to prepend
`devcontainer exec --workspace-folder .` to your compilation command and thus
runs the compilation command inside the container.

Usually you would mount your local working directory into your devcontainer.
Then you edit files just like without using devcontainer.  Then, launching any
kind of `compile` command, it is actually performed inside your devcontainer.


## Status

Development started a couple of months ago.  Since then it has been of big help
to me in my day job programming.  With my usual workflows, the packages just
does its job and otherwise stays out of my way.  As I don't tend to experiment
that much with edge cases, I might not be aware of caveats, when different
workflows are used.  This might especially the case if you are using `podman`
as container engine rather than `docker`.

If you experience any issues, please report them back in the issue tracker.

> [!IMPORTANT]
> Please consider this package experimental as long as there is no release yet.
> There might be breaking changes like changing function and variable names out
> of the blue.  So please consider checking the git history when updating.


## Usage

`devcontainer` provides the following commands:

* `devcontianer-mode` – a globally activated minor mode, that you can just have
  always activated.
* `devcontainer-up` – start the devcontainer of the current project.
* `devcontainer-restart` – stop and restart the devcontainer of the current
  project.
* `devcontainer-rebuild-and-restart` – stop the current project's devcontainer,
  delete its docker images and rebuild it.  This is useful, when you for
  example changed the `Dockerfile` of your devcontianer.
* `devcontainer-kill-container` – stop the current project's devcontainer.
* `devcontainer-remove-container` – remove the current project's devcontainer.
* `devcontainer-remove-image` – remove the current project's devcontainer image.
* `devcontainer-execute-command` – execute an arbitrary command inside the
  devcontainer.
* `devconatainer-kill-command` – kill the process launched by
  `devcontainer-execute-command`.
* `devcontainer-term` – launch a terminal inside the container.


### Forwarding commands that are not using `compile`

If you need to forward some process call into the devcontainer which is not
done by the `compile` command of Emacs, you can use
`devcontainer-advise-command` to prepend the `devcontainer exec` call in front
of your command.  If you are not sure if `devcontainer` is always available you
can use the following call, which modifies your command if `devcontainer` is
available and if the command modification is advisable.

```elisp
(funcall (or (symbol-function 'devcontainer-advise-command) #'identity) command)
```

### Using TRAMP

An alternative way of using the package is to edit the files inside the
container itself using Emacs' builtin
[TRAMP](https://www.gnu.org/software/tramp/) facility. There are pros and cons
to it.  In order to use it, you can let `devcontainer-mode` deactivated and use
the function `devcontainer-tramp-dired` to open a `dired` window inside the
container. Then you can open files of your project inside the container.

If `devcontainer-mode` is activated it refrains from advising `compile`
functions, if the current buffer is a file inside the devcontainer.


## Configuration

The following things are customizable at this point:

* `devcontainer-execute-outside-container` – a list of programs, not to be
  executed inside the container but on the host system. Used for things like
  `grep`.

* `devcontainer-post-startup-hook` – a hook variable that can provide functions
  that are called after the container has been started. They take a couple of
  arguments. See the functions' documentation for details.

  One example use case is to hook in the function `devcontainer-tramp-dired` to
  open a `dired` window inside the container right after the start of the
  container.

* `devcontainer-engine` – the container engine you want to use. Can be either
  `'docker` (default) or `'podman`

* There are `devcontainer-term-command`, `devcontainer-term-shell` and
  `devcontainer-term-environment` to customize the terminal launched by
  `devcontainer-term`.

> [!TIP]
> If you experience weird control sequence output due to the bash prompt inside
> the devcontainer, you can try modifying the `TERM` variable by
>
> ```elisp
> (setq devcontainer-term-environment '(("TERM" . "xterm-256color")))
> ```

## Installation

### Prerequisites

You need to have the [devcontainers/cli](https://github.com/devcontainers/cli)
command line tool installed into your execution path.  For that you need a
working `node`/`npm` setup.  Then you can install the tool with

```
npm install -g @devcontainers/cli
```

Of course you also need a working [docker](https://docker.com) installation and
probably also [docker compose](https://docs.docker.com/compose/).

### Installing the package

At the moment the most convenient method to install it is using
[straight.el](https://github.com/raxod502/straight.el). Put the following lines
into your startup file.

``` elisp
(use-package devcontainer
  :straight (devcontainer :type git :host github :repo "johannes-mueller/devcontainer.el"))
```

Then you have a bunch of commands prefixed with `devcontainer-` to have fun
with devcontainers. As of now they are not documented. Documentation will be
written once some practical experience is gained.

Maybe you also want to install the `coterm` package and activate `coterm-mode`.
Then the Docker output in the Emacs buffers gets way more readable.


## Plans

- [ ] See how it works in practice
- [ ] Fix things that don't work
- [ ] Document things that work
- [ ] See if we find a way to reliably connect to a language server inside the
      container.  Maybe find inspiration from `eglot-booster-mode`.
- [ ] See if we can get [dape](https://github.com/svaante/dape) to work with
  `devcontainer-mode`.
- [ ] See if anything more is needed that can be implemented with a reasonable
  effort.
- [ ] Announce it to [melpa](https://melpa.org/)
- [ ] Enjoy the `devcontainer` package in real life programming


## Contributing

At this stage of development it would be great if you share your thoughts on
the [discussion
page](https://github.com/johannes-mueller/devcontainer.el/discussions/).  If
you have something more concrete, you can for sure also file an issue.


## Development

This package uses unit tests defined by Emacs' built in
[ERT](https://www.gnu.org/software/emacs/manual/html_mono/ert.html) testing
framework. An easy way to run the tests is
[ert-runner.el](https://github.com/rejeep/ert-runner.el). To run the test suite
from the command line simply

* Install [Cask](https://github.com/cask/cask) on your system
* Open a shell in the repo's root directory
* Install the rependencies using
```
$ cask install
```
* Run the test suite using
```
$ cask exec ert-runner
```

If you want to run the tests interactively from within emacs, you might want to
take a look at
[test-cockpit.el](https://github.com/johannes-mueller/test-cockpit.el).
