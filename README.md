# csv2json

Command line utility to convert csv data to [jsonl](http://jsonlines.org/)
data. It utilizes streaming, so it's supposed to handle large csv files in
constant space.

```
csv2json < test.csv > test.json
```

See [nix/README.md](nix/README.md) to learn about how this project uses nix for
development.
