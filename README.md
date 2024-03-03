<h1 align="center">
    smh
</h1>

<p align="center">
    A string manipulation tool written in haskell.
</p>

---

smh aims to bring the power of optics into the command line. It provides a terse, domain specific language for various string manipulation tasks.

<div align="center">
    <a href="https://danryba253.github.io/smh-docs">Documentation</a>
    <a href="https://danryba253.github.io/smh-docs/getting-started">Getting Started</a>
    <a href="https://danryba253.github.io/smh-docs/user-guide">User Guide</a>
</div>

---

<h2>Intalling</h2>

* <h3>Arch linux</h3>

    `yay -S smh-bin`

* <h3>Any distro</h3>

    Download the latest [release](https://github.com/DanRyba253/smh/releases/latest)

<h2>Building</h2>

Refer to the [building instructions](https://danryba253.github.io/smh-docs/getting-started/#building) in the docs.

<h2>Examples of usage</h2>

* `smh 'words.if len>3|get' < input_file` - display all words longer than 3 characters   
* `smh 'words.[0]|over upper' "hello world"` - capitalize all words  
* `smh 'lines.if startsWith "*".<words>.[2]|get' < input_file` - display the third word in lines that start with an asterisk

<h2>Contributing</h2>

Before contributing, visit the [Contributing](https://danryba253.github.io/smh-docs/contributing/) page in the docs.

<h2>Acknowledgements</h2>

* Haskell, for being an awesome language
* lens library authors, for inspiring this whole ordeal
* mkdocs team, for making an awesome static documentation site generator
