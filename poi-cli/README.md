`poi-cli` is a simple restorable file removing command like [trash-cli](https://github.com/andreafrancia/trash-cli).

# Trash Can location
Of course `poi-cli` needs a location to save trashed file objects.

Default location is `$HOME/.poi/TRASH-CAN` in (maybe) all OS, but you can change it by some ways.

## configuration

In order of priority, trash can location settings methods are as follows:

### Environment variable
If `poi-cli` recognized that environment variable named `POI_TRASH_CAN_PATH` is set, `poi-cli` use the value of `POI_TRASH_CAN_PATH` as the trash can location.

### Configuration file
If `poi-cli` found a config file named `poi.yaml`, `poi-cli` refers the path from `poi.yaml`.

`poi.yaml` detail is [here](#poiyaml).

### Default location
If `poi-cli` could not any trash can location settings, `poi-cli` does his job in `$HOME/.poi/TRASH-CAN`.

# Trashed object
Trashed objects are possible to duplicated name.

To distinguish those, `poi-cli` use a number called `index`. `index` appears in functions depends on trash can itself like `poi list`, `poi delete`.

You know that `index` is inspirated from `trash-cli`.(Thanks for a lot `trash-cli`.)

# Functions

`poi-cli` provides some functions to control file objects lifecycle.

You can use glob pattern to specify a file path.

## put

If you want to remove some file objects safety, `poi put` works for you.

### command options

`poi put` equipments some options.

#### --burn

`--burn` deletes permanently target file objects without putting to trash, this is just same as `rm`.

Please be careful using.


#### --interact

`--interact` confirms that do you want to do it really.

Normally this option is helpless at all.

```sh
$ poi put --interact foo.txt
Do really? [y/n]> # y -> do it, n -> quit
```

But you can use this option to prevent the accidental `--burn`.

## restore

If you want to restore some file objcts from trash can, `poi restore` works for you.

If restoring trashed objects conflicted the destination path name, `poi-cli` provides a special prefix `poi-restore_` to restoring trashed objects.


Assume `a/b/foo.txt` indexed as `1` exists in original path, but `a/b/bar.zip` indexed `2` does not.

```sh
$ poi restore 1 2
# Then, maybe looks like:
$ ls a/b/
foo.txt             # original file
poi-restore_foo.txt # conflicted file
bar.zip             # non-conflicted file
```

### command options

`poi restore` equipments some options.

#### --force

`--force` option overwrites the conflicted file object rather than providing a special prefix.

```sh
$ poi restore --force 1
$ ls a/b/
foo.txt # This is resotred file object (This means rewinding to old version. Be careful.)
```

#### --interact

`--interact` confirms overwriting or prefixing for you.

```sh
$ poi resotre --interact 1
overwrite? [y/n/c]> # y -> overwriting, n -> prefixing, c -> cancel
```

`--interact` is most priority option, so `--force` option will be ignored when you use both of `--interact` and `--force`.

## list

If you want to look trashed file objects list, `poi list` works for you.

There is no option.

## delete

If you want to delete some file objects permanently, `poi delete` works for you.

### command options

`poi delete` equipments some options.

#### --deprecated

`--deprecated` allows you to specify a time range to delete bulky.

Permitted format is `{non-zero positive number}[d(ay)/h(our)/m(in)]|all`.

This value is included in the target time range. So:

```sh
$ poi --deprecated 9d # Delete all files which were trashed before than 9 days ago. (Target time range is `current time - 9 days <=`).
$ poi --deprecated 1h # Delete all files which were trashed before than 1 hour ago. (Target time range is `current time - 1 hour <=`)
$ poi --deprecated 10m # Delete all files which were trashed before than 10 minutes ago. (Target time range is `current time - 10 minutes <=`)
$ poi --deprecated all # Delete all files in trash can.
```

#### --interact

`--interact` confirms that do you want to do it really.

You can use this option to avoid the accidental deleting.

# poi.yaml

`poi.yaml` is a setting file for `poi-cli`.

`poi-cli` assumes that `poi.yaml` is at `$HOME/.poi-cli`, but you can use environment variable named `POI_SETTING_FILE_PATH` to change setting file path (The name of `poi.yaml` is immutable).

`poi.yaml` format is:

| field | type | description | default |
| ----- | ---- | ----------- | ------- |
| basedir | string | Required a valid directory path. If a directory path didn't exist, `poi-cli` creates it automatically. | `$HOME/.poi` |
| prefix | string | If restoring file objects conflict against original path, this value is applied as prefix. | `poi-restore_` |

All fields are optional. This means that fields not set value are applied `default` value.
