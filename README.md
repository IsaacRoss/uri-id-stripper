# URI "id" stripper

Working on a web project a few years back, we had a requirement to strip **ids**
out of the uri for the purpose of tracking. At the time this was done
imperitavely in JS, and later done more functionally with libraries such as
Ramda.js. I thought it would be an interesting beginner Haskell appliction.

To be considered an *id*, the segment of the URI must meet the following
criteria.
 - Must be alphanumeric
 - All characters must be uppercase
 - Must be longer than 16 characters


**Example:**
```
https://somewhere.com/projects/78378438473847HDJDHFJHFDFD343434/reports/8738748374837483742983749238472398432
```
is translated to
```
https://somewhere.com/projects/reports/
```

