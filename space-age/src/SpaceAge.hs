module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

secondsInEarthYear = 31557600
mercuryOrbitalPeriod = 0.2408467
venusOrbitalPeriod = 0.61519726
marsOrbitalPeriod = 1.8808158
jupiterOrbitalPeriod = 11.862615
saturnOrbitalPeriod = 29.447498
uranusOrbitalPeriod = 84.016846
neptuneOrbitalPeriod = 164.79132

ageOn :: Planet -> Float -> Float
ageOn Earth seconds   = seconds / secondsInEarthYear
ageOn Mercury seconds = ageOn Earth seconds / mercuryOrbitalPeriod
ageOn Venus seconds   = ageOn Earth seconds / venusOrbitalPeriod
ageOn Mars seconds    = ageOn Earth seconds / marsOrbitalPeriod
ageOn Jupiter seconds = ageOn Earth seconds / jupiterOrbitalPeriod
ageOn Saturn seconds  = ageOn Earth seconds / saturnOrbitalPeriod
ageOn Uranus seconds  = ageOn Earth seconds / uranusOrbitalPeriod
ageOn Neptune seconds = ageOn Earth seconds / neptuneOrbitalPeriod