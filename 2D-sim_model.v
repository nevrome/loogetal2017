import rand

fn main() {

  rand.seed(time.now().uni)

  // ###########################
  // ### CONSTANT PARAMETERS ###
  // ###########################

  x_min := -4000
  x_max := 4000
  y_min := -4000
  y_max := 4000

  // population parameters
  generations := 20001 // number of steps in random walk
  in_entities := 1 // initial number of entities
  max_entities := 10000	// carrying capacity - maximum number of entities the modelled domain can sustain

  // craniometric (CM) measurement parameters
  num_CM_meas := 50 // total number of CM measurements considered

  // gaussian distribution parameters - for CM measurements
  CM_mu := 1000 // mean
  CM_sigma_frac := 0.05 // standard deviation of CM gaussian - proportion of mean (CM_mu): 0.005, 0.05, 0.5
  CM_sigma_val := CM_mu*CM_sigma_frac // standard deviation of CM gaussian - value (CM_mu*CM_sigma_frac)

  prob_fis := 0.1 // probability with which each entity fissions at each generation
  prob_dep := 0.00001 // probability with which each entity is sampled (data recorded to output)

  // ###########################
  // ### VARIABLE PARAMETERS ###
  // ###########################

  // gaussian distribution parameters - for migration
  mut mig_mu := 0 // mean
  // mut mig_sigma = random.randint(1,125)	// 2.5 #					# standard deviation: 2.5, 5, 7.5, 10
  mut mig_sigma := 18.72 // runif(1, 0, 100)

  // ##################
  // ### INITIALISE ###
  // ##################

  //  initial (empty) array for storing population density values in
  mut entities := [0].repeat(generations)

  // initial positions
  x <- matrix(runif(entities, x_min, x_max), entities, 1)
  y <- matrix(runif(entities, y_min, y_max), entities, 1)

  x_original <- x
  y_original <- y

  // initial CM measurements
  CM_matrix <- matrix(CM_mu, entities, num_CM_meas)
  CM_sigma <- matrix(CM_sigma_val, num_CM_meas, 1)

}
