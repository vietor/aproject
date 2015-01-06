aproject
========

A simple project tool for Emacs.

This library allows the user to use Emacs on multiple projects.  
Any project has it's ".aproject" dirctory for store some files, like: session, desktop, etc.

Usag
-----

Add the following to your `init.el` (after the `aproject.el` in you load-path):

``` el
(require "aproject")
```

You can add custom process when aproject changed like:

``` el
(before-aproject-change
 (desktop-save aproject-storedir))

(after-aproject-change
 (setq desktop-path (list aproject-storedir))
 (desktop-read))
```

Variables
-------

**aproject-project** (ReadOnly)
> When aproject initialize like a **project** it t else nil.

**aproject-rootdir** (ReadOnly)
> The aproject working directory.

**aproject-storedir** (ReadOnly)
> The aproject store directory for store *project* specifec files.  
> Any project has itself store directory.

Functions
-------

**aproject-root-file (name)**
> Get file path in **aproject-rootdir**.

**aproject-store-file (name)**
> Get file path in **aproject-storedir**

Macros
-------

**before-aproject-change**
> Wrap aproject-before-change-hook, it should call before the aproject change
> **project**.  
> You can store some files for previous **project**.

**after-aproject-change**
> Wrap aproject-after-change-hook, it should call after the aproject change
> **project**.  
> You can read some files for current **project**.

License
-------

This program is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
Street, Fifth Floor, Boston, MA 02110-1301, USA.

