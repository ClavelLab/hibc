# hibc

The Human Intestinal Bacterial Collection

## Usage

```bash
# build the Dockerfile
docker build -t hibc-shiny .
# run the shiny app
docker run -it -d -p 3838:3838 hibc-shiny
```

## TODO

### Data/metadata

- [ ] merge the hibc metadata table and the assembly metadata table
- [ ] have the columns labelled in a machine-readable manner prefixed by categories: `cultivation_` or `genome_`
- [ ] prepare a translation table/vector from the machine-readable columns to the actual human-readable columns to be used for display
- [ ] IMPROVEMENT: describe the columns in a schemasheets/linkml metadata model
- [ ] IMPROVEMENT: display the data using the DataHarmonizer to ease edition/validation 

### Visualisation

- [ ] add an interactive scatter plot (plotly) of contamination-completion
- [ ] add a plot for the distribution of the genome size with two possible twists:
    - [ ] broken down by Phyla
    - [ ] with/without plasmids

### Search

- [ ] remove the panel with dropdown menus that is not subsetting anything
- [ ] include the code from the mi-atlas to summarise data on one isolate. This might involve restricting DT table selection to one only.


### misc

- [ ] replace the deprecated shinyforms package by mailtoR with a template, especially in the subject.
- [ ] name the tab of the website
- [ ] IMPROVEMENT: link the media to the mediadive website identifier.
