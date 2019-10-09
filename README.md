# WarfareCalculus
Application written in Haskell that utilizes the calculus of conventional war without Lanchester Theory

# Purpose
This program implements equations created by Joshua Epstein in the topic of warfare calculus. His research paper can be
found at https://www.brookings.edu/book/the-calculus-of-conventional-war/

# How to compile
Run GHCI with the command :l warfareCalculus.hs

# How to use
Run the program with the command 'main'.
Supply a variable and a day you wish to see the result for.

    Example use: agl 1

# Equation variable aliases.
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

# Initial Values

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
