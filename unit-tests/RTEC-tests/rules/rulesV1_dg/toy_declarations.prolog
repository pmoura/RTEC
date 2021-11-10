% Information about all our events and fluents.
% - Is each entity an event or a fluent?
% - Is it an input or an output entity?
% - Choose an argument to be used as index for quicker access.

:- dynamic((person/1, place/1)).

%%%%%%%%%%%%%%%%% INPUT EVENTS %%%%%%%%%%%%%%%%%
event(go_to(_,_)).
inputEntity(go_to(_,_)).
index(go_to(Person,_), Person).

event(lose_wallet(_)).
inputEntity(lose_wallet(_)).
index(lose_wallet(Person), Person).

event(win_lottery(_)).
inputEntity(win_lottery(_)).
index(win_lottery(Person), Person).

event(sleep_start(_)).
inputEntity(sleep_start(_)).
index(sleep_start(Person), Person).

event(sleep_end(_)).
inputEntity(sleep_end(_)).
index(sleep_end(Person), Person).

event(starts_working(_)).
inputEntity(starts_working(_)).
index(starts_working(Person), Person).

event(ends_working(_)).
inputEntity(ends_working(_)).
index(ends_working(Person), Person).

collectGrounds([lose_wallet(Person),
                win_lottery(Person),
                sleep_start(Person),
                sleep_end(Person),
                starts_working(Person),
                ends_working(Person),
                go_to(Person,_)],person(Person)).

collectGrounds([go_to(_,Y)],place(Y)).

%%%%%%%%%%%%%%%%% OUTPUT ENTITIES %%%%%%%%%%%%%%%%%
simpleFluent(location(_)=home).
outputEntity(location(_)=home).
index(location(Person)=home, Person).

simpleFluent(location(_)=pub).
outputEntity(location(_)=pub).
index(location(Person)=pub, Person).

simpleFluent(location(_)=work).
outputEntity(location(_)=work).
index(location(Person)=work, Person).

simpleFluent(rich(_)=true).
outputEntity(rich(_)=true).
index(rich(Person)=true, Person).

simpleFluent(rich(_)=false).
outputEntity(rich(_)=false).
index(rich(Person)=false, Person).

simpleFluent(working(_)=true).
outputEntity(working(_)=true).
index(working(Person)=true, Person).

simpleFluent(working(_)=false).
outputEntity(working(_)=false).
index(working(Person)=false, Person).

simpleFluent(sleeping(_)=true).
outputEntity(sleeping(_)=true).
index(sleeping(Person)=true, Person).

simpleFluent(sleepingHappy(_)=true).
outputEntity(sleepingHappy(_)=true).
index(sleepingHappy(Person)=true, Person).

sDFluent(happy(_)=true).
outputEntity(happy(_)=true).
index(happy(Person)=true, Person).

sDFluent(happy(_)=false).
outputEntity(happy(_)=false).
index(happy(Person)=false, Person).

simpleFluent(shappy(_)=true).
outputEntity(shappy(_)=true).
index(shappy(Person)=true, Person).

sDFluent(infiniteBeers(_)=true).
outputEntity(infiniteBeers(_)=true).
index(infiniteBeers(Person)=true, Person).

sDFluent(shortHappiness(_)=true).
outputEntity(shortHappiness(_)=true).
index(shortHappiness(Person)=true, Person).

sDFluent(drunk(_)=true).
outputEntity(drunk(_)=true).
index(drunk(Person)=true, Person).

sDFluent(sleeping_at_work(_)=true).
outputEntity(sleeping_at_work(_)=true).
index(sleeping_at_work(Person)=true, Person).

sDFluent(workingEfficiently(_)=true).
outputEntity(workingEfficiently(_)=true).
index(workingEfficiently(Person)=true, Person).



% How are the fluents grounded?
% Define the domain of the variables.

grounding(location(Person)=Place)          :- person(Person), place(Place).
dgrounded(location(_)=Place,place(Place)).
dgrounded(location(Person)=_,person(Person)).

grounding(rich(Person)=true)               :- person(Person).
dgrounded(rich(Person)=true,person(Person)).

grounding(rich(Person)=false)              :- person(Person).
dgrounded(rich(Person)=false,person(Person)).

grounding(happy(Person)=true)              :- person(Person).
dgrounded(happy(Person)=true,person(Person)).

grounding(happy(Person)=false)             :- person(Person).
dgrounded(happy(Person)=false,person(Person)).

grounding(shappy(Person)=true)             :- person(Person).
dgrounded(shappy(Person)=true,person(Person)).

grounding(working(Person)=true)            :- person(Person).
dgrounded(working(Person)=true,person(Person)).

grounding(working(Person)=false)           :- person(Person).
dgrounded(working(Person)=false,person(Person)).

grounding(sleeping_at_work(Person)=true)   :- person(Person).
dgrounded(sleeping_at_work(Person)=true,person(Person)).

grounding(sleeping(Person)=true)           :- person(Person).
dgrounded(sleeping(Person)=true,person(Person)).

grounding(sleepingHappy(Person)=true)      :- person(Person).
dgrounded(sleepingHappy(Person)=true,person(Person)).

grounding(workingEfficiently(Person)=true) :- person(Person).
dgrounded(workingEfficiently(Person)=true,person(Person)).

grounding(infiniteBeers(Person)=true)      :- person(Person).
dgrounded(infiniteBeers(Person)=true,person(Person)).

grounding(shortHappiness(Person)=true)     :- person(Person).
dgrounded(shortHappiness(Person)=true,person(Person)).

grounding(drunk(Person)=true)              :- person(Person).
dgrounded(drunk(Person)=true,person(Person)).



% In what order will the output entities be processed by RTEC?

cachingOrder(location(_)=home).
cachingOrder(location(_)=pub).
cachingOrder(location(_)=work).
cachingOrder(sleeping(_)=true).
cachingOrder(rich(_)=true).
cachingOrder(rich(_)=false).
cachingOrder(working(_)=true).
cachingOrder(working(_)=false).
cachingOrder(sleeping_at_work(_)=true).
cachingOrder(workingEfficiently(_)=true).
cachingOrder(happy(_)=true).
cachingOrder(happy(_)=false).
cachingOrder(sleepingHappy(_)=true).
cachingOrder(shappy(_)=true).
cachingOrder(infiniteBeers(_)=true).
cachingOrder(shortHappiness(_)=true).
cachingOrder(drunk(_)=true).

