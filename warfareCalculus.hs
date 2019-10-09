{-
    Title: Warfare Calculus
    Author: Brian Klarman
    Class: CS355
    Professor: Morris

    This program implements the equations of warfare calculus derived from Joshua Epstein.

    How to use:
    Run the program with the command 'main'.
    Supply a variable and a day you wish to see the result for. Variable names and descriptions can be found
    in the section titles "Equation Variable Aliases".

    Example use: agl 1

    Time in days, t = 1,2,3...

    **********************************************************
                    Equation Variable Aliases
    **********************************************************
    Attackers ground lethality surviving at start of (t)th day
        agl: A(sub(g))(t)

    Attackers ground prosecution rate per day, 0 <= alpha(sub(g)) <= 1
        agp: alpha(sub(g))(t)

    Attackers ground-to-ground lethality attrition rate per day (no-airpower case), 0 <= alpha(t) <= 1
        aggl: alpha(t)

    Attackers total ground-lethality attrition rate per day (air and ground-induced), 0 <= alpha(sub(a))(t) <= 1
        atl: alpha(sub(a))(t)

    Attackers threshold, or equilibrium, attrition rate; the value of alpha(sub(a))(t) the Attacker seeks to achieve and sustain, 0 < alpha(sub(aT)) <= 1
        at: alpha(sub(aT))

    Defender's ground lethality surviving at start of (t)th day
        dgl: D(sub(g))(t)

    Attackers ground lethality killed per Defender's ground lethality kill (average ground-to-ground casualty-exchange ratio)
        avgggce: P

    Defenders threhold attrition rate; the value beyond which withdrawal begins, 0 <= alpha(sub(dT))
        dt: alpha(sub(dT))

    Defender's total ground-lethality attrition rate per day (air and ground-induced), 0 <= alpha(sub(d))(t) <= 1
        dtl: alpha(sub(d))(t)

    Defenders rate of withdrawal in kilometers per day
        withd: W(t)

    Defenders maximum rate of withdrawal
        wmax: W(sub(max))

    Defenders close air support aircraft surviving at start of (t)th day
        dcas: D(sub(a))(t)

    Defenders CAS aircraft attrition rate per sortie, 0 <= alpha(sub(da)) <= 1
        dcasa: alpha(sub(da))

    Defenders CAS daily sortie rate
        dsort: S(sub(d))

    Attackers armored fighting vehicles killed per Defender CAS sortie
        afvk: K(sub(d))

    Attackers CAS aircraft surviving at start of (t)th day
        acas: A(sub(a))(t)

    Attackers CAS aircraft attrition rate per sortie, 0 <= alpha(sub(aa)) <= 1
        acasa: alpha(sub(aa))

    Defenders AFVs killed per Attacker CAS sortie
        dafvk: K(sub(a))

    Attackers CAS daily sortie rate
        asort: S(sub(a))

    AFVs per division equivalent
        vde: V

    Lethality points per DE
        lde: L

    Defensive ground lethality killed by Attackers CAS on the (t)th day
        acast: acas(t)

    Attacking ground lethality killed by Defenders CAS on the (t)th day
        dcast: dcas(t)
    ----------------------------------------------------------

    **********************************************************
                        Initial Values
    **********************************************************

    The following must have initial values set. Parenthetical values represent
    example used from warfare_calculus.pdf

        Variable                        Init Value      Variable Name
        --------                        --------        -------------
        A(sub(g))(1)                    330,000         init_agl
        alpha(sub(g))(1)                0.020           init_agp
        D(sub(g))(1)                    200,000         init_dgl
        AvgGGCE: P                      1.50            init_avgggce
        alpha(sub(dT))                  0.050           init_dt
        alpha(sub(aT))                  0.075           init_at
        W(sub(max)                      20.00           init_wmax
        D(sub(a))(1)                    300             init_dcas
        alpha(sub(da))                  0.05            init_dcasa
        S(sub(d))                       1.50            init_dsort
        K(sub(d))                       0.50            init_afvk
        A(sub(a))(1)                    250             init_acas
        ACASA : alpha(sub(aa))          0.05            init_acasa
        K(sub(a))                       0.25            init_dafvk
        S(sub(a))                       1.00            init_asort
        V                               1,200           init_vde
        L                               47,490          init_lde
    ----------------------------------------------------------
-}

import Data.List
import System.IO

{-
    Definitions for our starting values to run the
    equations.
-}
init_agl :: Double
init_agl = 330000.00

init_agp :: Double
init_agp = 0.020

init_dgl :: Double
init_dgl = 200000.00

init_avgggce :: Double
init_avgggce = 1.50

init_dt :: Double
init_dt = 0.050

init_at :: Double
init_at = 0.075

init_wmax :: Double
init_wmax = 20.00

init_dcas :: Double
init_dcas = 300.00

init_dcasa :: Double
init_dcasa = 0.05

init_dsort :: Double
init_dsort = 1.50

init_afvk :: Double
init_afvk = 0.50

init_acas :: Double
init_acas = 250.00

init_acasa :: Double
init_acasa = 0.05

init_dafvk :: Double
init_dafvk = 0.25

init_asort :: Double
init_asort = 1.00

init_vde :: Double
init_vde = 1200.00

init_lde :: Double
init_lde = 47490.00

init_w :: Double
init_w = 0.00

-- *******************************************************************
{-
    Formula (A-13)
    Calculates Attackers CAS aircraft surviving at start of (t)th day
-}
acas :: Int -> Double
acas t =
    let x = 1 - init_acasa
        y = init_asort * (fromIntegral t -1)
        in init_acas * x**y
-- *******************************************************************

-- *******************************************************************
{-
    Formula (A-12)
    Calculates Defenders CAS aircraft surviving at start of (t)th day
-}
dcas :: Int -> Double
dcas t =
    let x = 1 - init_dcasa
        y = init_dsort * (fromIntegral t-1)
        in init_dcas * x**y
-- *******************************************************************

-- *******************************************************************
{-
    Formula (A-11) - Cleared of summation
    Calculates defensive ground lethality killed by Attacker's CAS
    on the tth day.
-}
acast :: Int -> Double
acast t =
    let lv = init_lde/init_vde
        x = (1 - init_acasa)
        y = init_asort * (fromIntegral (t-1))
        z = (init_asort + 1)
        in lv * init_acas * (x**y) * init_dafvk * ((1 - (x**z) - init_acasa) / init_acasa)
-- *******************************************************************

-- *******************************************************************
{-
    Formula (A-10) - Cleared of summation
    Calculates attacking ground lethality killed by Defender's CAS
    on the tth day.
-}
dcast :: Int -> Double
dcast t =
    let lv = init_lde/init_vde
        x = (1 - init_dcasa)
        y = init_dsort * (fromIntegral (t-1))
        z = (init_dsort + 1)
        in lv * init_dcas * (x**y) * init_afvk * ((1 - (x**z) - init_dcasa) / init_dcasa)
-- *******************************************************************   

-- *******************************************************************
{-
    Formula (A-7)
    Calculates the Attacker's threshold, or equilibrium, attrition rate;
    the valueof which the Attacker seeks to achieve and sustain.
-}
atl :: Int -> Double
atl t =
    (((agl t) - (agl (t+1)))/(agl t))
-- *******************************************************************

-- *******************************************************************
{-
    Formula (A-6)
    Calculates Attacker's ground prosecution rate per day.
-}
agp :: Int -> Double
agp t
    | t <= 1 = init_agp
    | otherwise = (agp (t-1)) - ((init_at - (agp (t-1)))/init_at) * ((atl (t-1)) - init_at)
-- *******************************************************************

-- *******************************************************************
{-
    Formula (A-5)
    Calculates Defender's total ground lethality attrition rate per day (air and
    ground induced).
-}
dtl :: Int -> Double
dtl t =
    (((dgl t) - (dgl (t+1)))/(dgl t))
-- *******************************************************************

-- *******************************************************************
{-
    Formula (A-4)
    Calculate's Defender's rate of withdrawal in kilometers per day.
-}
withd :: Int -> Double
withd t = 
    if t >= 1 
            then if (dtl (t-1)) <= init_dt then 0
            else (withd (t-1)) + (((init_wmax) - withd(t-1))/(1 - init_dt)) * ((dtl (t-1)) - init_dt)
    else 0
-- *******************************************************************

-- *******************************************************************
{-
    Formula (A-3)
    Calculates Attacker's ground to ground letahlity attrition rate per day
-}
aggl :: Int -> Double
aggl t =
    (agp t) * (1 - ((withd t) / init_wmax))
-- *******************************************************************

-- *******************************************************************
{-
    Formula (A-2)
    Calculates Defender's ground lethality surviving at start of tth day.
-}
dgl :: Int -> Double
dgl t
    | t <= 1 = init_dgl
    | otherwise = (dgl (t-1)) - ((aggl (t-1))/init_avgggce) * (agl (t-1)) - (acast (t-1))
-- *******************************************************************

-- *******************************************************************
{-
    Formula (A-1)
    Calculates Attacker's ground lethality surviving at start of tth day.
-}
agl :: Int -> Double
agl t
    | t <= 1 = init_agl
    | otherwise = (agl (t-1)) * (1 - (aggl (t-1))) - dcast (t-1)
-- *******************************************************************

main = do 
    putStrLn "*****************************"
    putStrLn "* Program: Warfare Calculus *"
    putStrLn "*   Author: Brian Klarman   *"
    putStrLn "*****************************"
    putStrLn "Supply a variable and a day you wish to see the result for."
    putStrLn "Valid variables are: agl, dgl, aggl, withd, dtl, agp, atl, dcast, acast, dcas, acas"
    putStrLn "To obtain the integer values displayed in table A-2 of research paper, use round"
    putStrLn "Example: round(agl 1)"