[![Tests](https://github.com/johannes-mueller/devcontainer-mode/actions/workflows/test.yml/badge.svg)](https://github.com/johannes-mueller/devcontainer-mode/actions/workflows/test.yml)

# devcontainer.el

Rudimentary devcontainer support for Emacs


## Synopsis

This package lets you handle i.e. start, restart devcontainers of your projects
and forwards all your `compile` commands into the devcontainer by advising
`compilation-start`.  So as long you use `compile` to build, test and run
(parts of your) software, all will be executed inside the devcontainer.


## Status

Development started a couple of weeks ago.  Since then, my experiences in
practice are positive.  So I would like to encourage anyone interested in this
to already use it and report back you experiences in the
[issues](https://github.com/johannes-mueller/devcontainer-mode/issues)

Probably there are features missing.  So also feel free to discuss your wishes
and ideas.


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

### Configuration

As of now, there is not much configuration to be done.

* `devcontainer-execute-outside-container` – a list of programs, not to be
  executed inside the container but on the host system. Used for things like
  `grep`.


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
  :straight (devcontainer :type git :host github :repo "johannes-mueller/devcontainer-mode"))
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
page](https://github.com/johannes-mueller/devcontainer-mode/discussions/).  If
you have something more concrete, you can for sure also file an issue.
