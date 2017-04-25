# Holiday-inator-3000

This is a rough version of a cheap flights finder which, given a starting date and the number of weeks will search for flights with the following ranges:

(Thu,Sun) , (Wed,Sun) , (Thu,Mon) , (Fri,Mon) , (Fri,Tue), (Fri,Sun) , (Sat,Mon)

(defined in the `Lib.hs` file under `possibleWeekends`)

##Usage

ATM the whole thing is very rough. The app uses Skyscanner API to get flight and cost information. In order to use, you must first obtain a Skyscanner API key. Replace:

```haskell
apiKey :: Maybe ApiKey
apiKey = Nothing
```

with:

```haskell
apiKey :: Maybe ApiKey
apiKey = Just "<your-key-here>"
```

in the `Lib.hs` file.

To run, use ghci, calling `stack ghci`. Then do:

1) `runP <no_of_weeks> <date>` where `<no_of_weeks>` is the number of weeks/weekends to retrieve data for and `<date>` is the initial date, passed in as a string with the format "YYYY-mm-dd". I.e. to retrieve for flights for the next 3 weeks, starting on the 1st of May, 2017, do: `runP 3 "2017-05-01"`

2) once `runP` finishes, it will save the results into a JSON file called `out.json`. To get cheapest flight, load up the JSON file by typing `db <- loadDB "./out.json"` into the ghci REPL and then call `queryDB db <max_price>`, where `queryDB db 40` will show return flights to anywhere, from London Luton/Stansted, Birmingham International or East Midlands Airport for under £40.

3) TODO: be able to change source/destination airports :P
