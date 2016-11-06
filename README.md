# wldquery

`wldquery` is a tool for querying information from a Terraria world file header.

## Usage 

The general format is

`./wldquery [wldpath] [key1 key2 ...]`

where `[wldpath]` is the path to your world file and `[key1 key2 ...]` are a
list of keys you are querying.

## Keys

See [this list of keys](keys.txt) that you can query from the world file header.

## Examples

Assuming you are querying the world ID on windows:

`wldquery.exe C:\path\to\your\file.wld world_id`

You can also query multiple keys. Each value will be printed on its own line:

`wldquery.exe C:\path\to\your\file.wld world_name max_tiles_x max_tiles_y`

Output:

```
SampleWorld
4200
1200
```
