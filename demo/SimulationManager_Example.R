# Example for SimulationManager

# construct Portfolio
ptf <- Portfolio()

# design tree structure
Bilanz = Tree(list(
  branches = list( 
    Bilanz = c("Aktiva","Passiva"),
    Aktiva = c("FixeDarlehen", "VariableDarlehen"),
    Passiva = c("Interbank","Kundenkonten")
  ),
  leafs=list(
    FixeDarlehen = c("ID1","ID2"),
    VariableDarlehen = c("ID3","ID4"),
    Interbank = c("ID5"),
    Kundenkonten = c("ID6")
  )
))


# get a yield curve and add it to the risk factor connector
rf_conn <- RiskFactorConnector()

# define the time buckets to use.
tb <- NA

# define the strategy
strategie = diag(c(1.05, 1.05, 1.05, 1.05))
colnames(strategie) = c("Kundenkonten", "Interbank", "FixeDarlehen", "VariableDarlehen")
rownames(strategie) = colnames(strategie)


# define templates 
templates = list(
  Kundenkonten=get(ptf[[Bilanz$leafs$Kundenkonten[1]]],"all"),
  Interbank= get(get(ptf,as.character(Bilanz$leafs$Interbank[1])),"all"),
  FixeDarlehen=get(get(ptf,as.character(Bilanz$leafs$FixeDarlehen[1])),"all"),
  VariableDarlehen=get(get(ptf,as.character(Bilanz$leafs$VariableDarlehen[1])),"all")
)

# create the simulation manager
sim.manager <- SimulationManager(Portfolio = ptf,
                                 TreeStructure = Bilanz,
                                 RiskFactors = rf_conn,
                                 TimeBuckets = tb,
                                 Strategy = strategie,
                                 Templates = templates)

# simulate the strategy
simulate(sim.manager, start_date = "2012-12-31", end_date = "2020-12-31")








