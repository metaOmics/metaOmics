# metaOmics

## File organization

```
├── README.md       # this file
├── WWW             # folders for network resources
│   ├── css         # all css files
│   └── js          # all javascript files
├── global          # folders for all global data
│   ├── constants.R # all global constants
│   └── database.R  # database related class and helper
├── global.R        # Shiny's env setup file, will be
                    # executed before app start up
├── server          # folder for all server modules
│   ├── ...
├── server.R        # Shiny's default server file
├── ui              # folder for all ui modules
│   ├── ...
└── ui.R            # Shiny's default ui file
```
