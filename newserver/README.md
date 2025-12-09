# DISCLAIMER

1. All the `*.js` files are generated using rescript
2. Rescript is a statically typed language but folks don't need to use it, I am using it because I want a filter
3. All the files generated can be used to import my code.
4. to run this I am using `mise`, Installing that and adding bun through it would be nice because it would isolate all the package installers from the global installers. <br><br>

# INSTALLATION
<br>

### LINUX/MAC (recommended)

The following way is what I use but if folks have their own workflows, they can choose them.
```bash
curl https://mise.run | sh
```

### BREW
```bash
brew install mise
```

## WINDOWS
Use the package installers for windows, one can choose from the following
- #### WINGET
    ```ps1
    winget install jdx.mise
    ```
- #### SCOOP
    ```ps1
    scoop install mise
    ```
- #### CHOCOLATEY
    ```ps1
    choco install mise
    ```
## Others
[installations instruction](https://mise.jdx.dev/getting-started.html) for other methods.

<br>

# BUN

## INSTALLATION

I am going to install `bun` using `mise` by using `mise use bun`. It will create a `mise.toml` and all the child directories will implicitly use packages mentioned inside `mise.toml`.<br><br>

I would suggest folks to use `mise exec <package-name> -- <package-name> [...]`. All the stuff after `--` would be the same as using the specific package using `package-name` directly from bash.<br><br>

Mise provides an alternative shell environment nothing else. This helps in isolation of packages and the dependencies installed using those packages.
<br><br>

There are some packages which are not supported by mise but my current estimation is things might just work.<br><br> 

## USAGE
- I am using `bun` to run a `res:build` script (in `package.json`). It essentially run the rescript watcher - `mise exec bun -- bunx rescript watch`
- Secondly for running the `src/server.ts` file to run the server by `mise exec bun -- bun run src/server.ts`

# RESCRIPT
- TODO
    - already talked about the why, talk about how