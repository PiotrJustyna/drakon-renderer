# 2024-08-09

I have implemented basic error handling, but having glimpsed into the rabbit hole of Haskell exceptions I am positive that this is not something I want to explore at the moment. I think I could just get away with using simpler techniques (`Maybe` or `Either` monads instead of exceptions) and focus my attention instead on more valuable things like setting up a home server (I want this renderer to be a service) on my raspberry pi. I'm planning to focus on it for the time being and to gather my thoughts on a smooth transition from the `Icon` type to a new type that will represent positioned icons. For positioning I'm planning to use a simple cartesian plane - positive x corrdinates and negative y coordinates only.

# 2024-08-02

Reading drakon diagrams from json files is now working fine, or at least the happy path is. What I think would make sense now is a degree of error handling:

* starting from obvious basics like exception hangling - file missing, file larger than some sort of fixed max size, cannot deserialize, etc.
* to more advanced validation like:
    * length of a certain property is incorrect
    * icon dependencies identify icons not present in the diagram

This validation should be extensible (maybe a flexible group of predicates?) so that I can easily add more validation rules in the future.

# 2024-07-29

I am currently experimenting with different ways of providing diagram input reasonably easily. This is in order to be able to easily test various types of diagrams and spot any errors or inefficiencies with the renderer.

I have a reasonably well fleshed out renderer in place already but I'm never happy with it, so we can definitely expect some aspects of it to be reworked in the future.