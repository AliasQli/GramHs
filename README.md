# GramHs

A GUI for [Mirai](https://github.com/mamoe/mirai) written in Haskell. Also relies on [Mirai HTTP API](https://github.com/project-mirai/mirai-api-http).

The GUI is based on Gtk. Special thanks to [haskell-gi](https://github.com/haskell-gi/haskell-gi) and [gi-gtk-declarative](https://github.com/owickstrom/gi-gtk-declarative).

## License

Since it's a tradition in the Haskell community, this project is open-sourced under the BSD3 License instead of the suggested AGPL License. See [LICENSE](./LICENSE) for more details.

## How to Build

```shell
stack build
```

or

```shell
stack install
```

## How to Use

1. Setup Mirai. 

    The project contains a shell script to set up Mirai easily (`setup-mirai.sh`). It downloads Mirai `jar`s and generates a `start.sh` for you. After that what you have to do is (files mentioned below are generated after first run):

    - Edit `mirai/config/Console/AutoLogin.hs` and add your account and password. 
    
        If this doesn't work, you may have to add your account directly in the console using the `/login` command.

    - Edit `mirai/config/MiraiApiHttp/setting.yml` and:

        - Set `port` and `authKey`.

        - **Manually change `enableWebsocket` to `true`.**

2. Setup GramHs.

    Put a `gramhs.json` file in your `/etc/` folder. It should be like (delete the comments):

    ```json
    {
      // IP address of the Mirai server. Often your local machine.
      "baseUrl": "127.0.0.1",
      // The port you just set.
      "miraiPort": 1234,
      // The authKey you just set.
      "authKey": "authKey",
      // Your qq number added to AutoLogin.hs.
      "qq": 1234567890
    }
    ```

    No field should be omitted.

3. Run `start.sh` and then the executable.

4. If an error occurs saying a `.so` file cannot be found, just locate which package it's in and install the package. Meanwhile, you can [report it](https://github.com/AliasQli/GramHs/issues) so I can update this `README.md` and specify the needed package here.

5. If you should encounter any bug while using GramHs, please first check if it's caused by Mirai or by this program and then report it.
