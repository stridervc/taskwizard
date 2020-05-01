Taskwizard is a to do list manager for the Linux console.
It is heavily inspired by [Taskwarrior](https://taskwarrior.org/).
I started this project while learning Haskell, and it has been a
good exercise for learning.

Caution
=======
Taskwizard is a work in progress, and in early stages. As such,
it can (and probably will) make changes that break compatibility
with previous save files. Please do not rely on it for anything
important and make a backup of `~/.taskwizard` before upgrading
to a newer version. The save file is plain text and should be
quite easy to understand and even edit with care. As some features,
like editing, are not available yet, this might even be necessary
at times.

Installation
============
In order to build Taskwizard, you will need to install the Haskell stack.
This can be done by the script available at [get.haskellstack.org](https://get.haskellstack.org/).
This script should be run as a non root user. It contains comments with
more information.

After installing the Haskell stack, clone the repository to a suitable
directory and build and install. The build will probably take a long time
the first time, as the Haskell stack will download and build all dependencies.
```
git clone git@github.com:stridervc/taskwizard.git
cd taskwizard
stack build
stack install
```
The binary `taskwizard` should then be available in `~/.local/bin/`
This directory can be added to your PATH, or you can copy the binary to
a location that is in your path.

Usage
=====
I alias `taskwizard` to `t`, because I use it frequently. I recommend
you add this alias in your `~/.bashrc`. 

Let's create and work with some tasks to see Taskwizard's (basic, for now)
functionality.
```
alias t=taskwizard
t add Learn Taskwizard
t
```
This will create and then list your one task. The number on the right is a
priority calculated by Taskwizard. Tasks are sorted according to this priority.
The number on the left is an ID number that you can use to refer to tasks.

```
t add Buy a broom
t add Sweep the floor depends:2
t
```
You'll see that 'Sweep the floor' is lower priority than 'Buy a broom'.
That is because 'Sweep the floor' depends on 'Buy a broom', which means
it cannot be done before you've completed buying a broom.

```
t 2 start
t
```
When you start a task, you can mark it as started, which increases it's priority
so that it moves up the list. It's also marked a different colour, making it
easy to see what you were busy with before.

```
t add This is just a test
t 2 done
t
t 4 delete
t
```
You'll notice that completing task 2 did not change the ID number of task 4. This
is something I wanted to do differently from Taskwarrior, which renumbers tasks as
they get completed or removed.

```
t 3 done
t add Buy food project:home priority:10
t
```
It's also possible to set a baseline priority manually, to move a task up the list
immediately. Specifying which project a task is part of can help to group them. At
the moment this is visual only, but more features around this will come.
You'll also notice that the new task doesn't reuse a previous ID, this is also a
deliberate feature. A bit more on this in the Design section.

```
t 5 delete
t 1 done
```

Design
======
As mentioned in the usage section, one of the design goals was to not change or
reuse task ID numbers, so that if you do remember a task ID, you can safely assume
that it hasn't changed.

If you've followed the tutorial above, and take a look at the contents of `~/.taskwizard`,
you'll see that it isn't empty, even though your task list is. Instead of storing your tasks,
Taskwizard stores the actions you take on those tasks. This is so that undo/redo can be
implemented in future.

There is a refactor command (`t refactor`) which will remove completed and deleted tasks
from the save file, and also renumber tasks. When undo is implemented, it will probably
not work further back than a refactor. But refactor can be used if the save file gets
too big and/or task ID numbers become too large. This of course means that task IDs will be
reused after a refactor.

Broken things
=============
The following is a list of things that should be, and are planned to be implemented.
* `t <id>` should assume list, like `t` does
* No undo and redo
* No modifying of tasks
* Dependency handling is very basic at the moment
* Should be able to filter on project, and other filtering improvements
* Many more...

