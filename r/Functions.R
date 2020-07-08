library(tidyverse)
# pop() ========================================================================

pop <- function(n, split = 1, success1, success0 = success1){
  argcheck <- c(split, success1, success0)
  if (any(argcheck > 1) | any(argcheck < 0)){
    stop (
      paste0("1 or more arguments that must contain proportions or",
             " probabilities are < 0 or > 1."),
      call. = FALSE
    )
  }
  if ((split != 1 | split != 0) & success0 == success1){
    warning (
      paste0("Population split into two groups, but both groups have same",
             " probability of harvest. Consider specifying argument",
             " 'success0'."),
      call. = FALSE
    )
  }
  if ((split == 1 | split == 0) & success0 != success1){
    warning(
      paste0("Population is not 'split' into different groups, and 'success1'",
             " does not equal 'success0'. To simulate different harvest",
             " rates, the population must be 'split' into two groups."),
      call. = FALSE
    )
  }
  pop <- tibble::tibble(
    pop_size     = n,
    group        = rbinom(n, 1, split),
    harvest      =  case_when(
      group == 1 ~ rbinom(n, 1, success1),
      group == 0 ~ rbinom(n, 1, success0)),
    true_harvest = sum(harvest),
  )
  pop <- select(pop, pop_size, true_harvest, tidyselect::everything())
  return(pop)
}

# mand() =======================================================================

mand <- function(pop, resp, times = 1){
  if (any(resp > 1) | any(resp < 0)) {
    stop ("'resp' must be a proportion.", call. = FALSE)
  }
  n <- pop$pop_size[[1]]
  survsim <- function(fillthis){
    full_sim <- purrr::map_dfr(seq_along(resp), ~ pop) %>%
      mutate(
        method    = "mandatory",
        resp_rate = rep(resp, each = n),
        init_resp =
          case_when(
            harvest == 1 ~ rbinom(n*length(resp), 1, resp_rate),
            harvest == 0 ~ 0L
          )
      )
    full_sim <- full_sim %>%
      select(method, pop_size, true_harvest, tidyselect::everything())
    return(full_sim)
  }
  out <- purrr::map(1:times, survsim)
  names(out) <- paste("Response sim", 1:length(out))
  return(out)
}

# simple() =====================================================================

simple <- function(pop, sample, resp, bias,
                   fus = FALSE, fus_scale = NULL, times = 1) {
  argcheck <- c(sample, resp)
  if (any(argcheck > 1) | any(argcheck < 0)) {
    stop ("'sample' and 'resp' must only contain probabilities.",
          call. = FALSE
    )
  }
  if (fus & is.null(fus_scale)) {
    stop ("If 'fus' = TRUE, 'fus_scale' argument must be defined.",
          call. = FALSE)
  }
  if (!fus & !is.null(fus_scale)) {
    stop ("'fus_scale' is defined, but 'fus' = FALSE.",
          call. = FALSE)
  }
  if (!is.null(fus_scale)) {
    if (fus_scale > 1) {
      warning(
        paste0("fus_scale > 1; Hunters more likely to respond to follow up",
               " than to initial survey."),
        call. = FALSE
      )
    }
  }
  if (any(bias < 1)) {
    message(
      paste0("At least 1 value of 'bias' < 1; successful hunters will be less",
             " likely to respond to survey than unsuccessful hunters.")
    )
  }
  # used when scaling arguments scale probabilities to > 1:
  changeto1 <- function(x) {
    case_when(
      x > 1 ~ 1,
      TRUE  ~ x
    )
  }

  # survsim() ==================================================================
  n <- pop$pop_size[[1]]
  survsim <- function(fillthis){
    full_sim <- purrr::map_dfr(1:(length(resp) * length(bias)), ~ pop)
    full_sim <- mutate(
      full_sim,
      method        = "simple",
      sample        = rbinom(nrow(full_sim) , 1, sample),
      resp_bias     = rep(bias, each = n * length(resp)),
      uns_resp_rate = rep.int(rep(resp, each = n), length(bias)),
      suc_resp_rate = rep.int(rep(resp, each = n), length(bias)) * resp_bias,
      suc_resp_rate = changeto1(suc_resp_rate),
      init_resp =
        case_when(
          sample  == 0 ~ NA_integer_,
          harvest == 1 ~ rbinom(n * (length(resp) * length(bias)),
                                1,
                                suc_resp_rate),
          harvest == 0 ~ rbinom(n * (length(resp) * length(bias)),
                                1,
                                uns_resp_rate)
        )
    )
    if (fus) {
      full_sim <- mutate(
        full_sim,
        fus_uns_resp_rate = uns_resp_rate * fus_scale,
        fus_uns_resp_rate = changeto1(fus_uns_resp_rate),
        fus_suc_resp_rate = suc_resp_rate * fus_scale,
        fus_suc_resp_rate = changeto1(fus_suc_resp_rate),
        fus_resp =
          case_when(
            sample    == 0  ~ NA_integer_,
            init_resp == 1  ~ NA_integer_,
            harvest   == 1  ~ rbinom(n * (length(resp) * length(bias)),
                                     1,
                                     fus_suc_resp_rate),
            harvest   == 0  ~ rbinom(n * (length(resp) * length(bias)),
                                     1,
                                     fus_uns_resp_rate)
          )
      )
    }
    full_sim <- full_sim %>%
      select(method, pop_size, true_harvest, tidyselect::everything())
    return(full_sim)
  }
  # Output =====================================================================
  out <- purrr::map(1:times, survsim)
  names(out) <- paste("Response sim", 1:length(out))
  return (out)
}

# vol() ========================================================================

vol <- function(pop, resp, bias, fus = FALSE,
                fus_scale = NULL, fus_sample = NULL, times = 1) {

  argcheck <- c(fus_sample, resp)
  if (any(argcheck > 1) | any(argcheck < 0)){
    stop (
      paste0("1 or more arguments that must contain proportions or",
             " probabilities are < 0 or > 1. See ?pop"),
      call. = FALSE
    )
  }
  if (fus & (is.null(fus_scale) | is.null(fus_sample))){
    stop (
      paste0("If 'fus' = TRUE, 'fus_scale' and 'fus_sample' arguments",
             " must be defined.")
      , call. = FALSE
    )
  }
  if (!fus & (!is.null(fus_scale) | !is.null(fus_sample))){
    stop ("'fus_scale' and/or 'fus_sample' are defined, but 'fus' = FALSE.",
          call. = FALSE)
  }
  if (!is.null(fus_scale)){
    if (fus_scale < 1){
      message(
        paste0("fus_scale < 1; Hunters less likely to respond to follow up",
               " than to voluntarily report")
      )
    }
  }
  if (any(bias < 1)) {
    message(
      paste0("At least 1 value of 'bias' < 1; successful hunters will be",
             " less likely to respond to survey than unsuccessful hunters.")
    )
  }
  # used when scaling arguments scale probabilities to > 1:
  changeto1 <- function(x){
    case_when(
      x > 1 ~ 1,
      TRUE  ~ x
    )
  }

  # survsim()===================================================================
  lb <- length(bias)
  lr <- length(resp)
  n  <- pop$pop_size[[1]]
  survsim <- function(fillthis){
    full_sim <- purrr::map_dfr(1:(lr * lb), ~ pop) %>%
      mutate(
        method        = "voluntary",
        resp_bias     = rep(bias, each = n * lr),
        uns_resp_rate = rep.int(rep(resp, each = n), lb),
        suc_resp_rate = rep.int(rep(resp, each = n), lb) * resp_bias,
        suc_resp_rate = changeto1(suc_resp_rate),
        init_resp     =
          case_when(
            harvest == 1 ~ rbinom(n * lr * lb, 1, suc_resp_rate),
            harvest == 0 ~ rbinom(n * lr * lb, 1, uns_resp_rate)
          )
      )
    if (fus){
      full_sim <- full_sim %>%
        mutate(
          fus_uns_resp_rate = uns_resp_rate * fus_scale,
          fus_suc_resp_rate = suc_resp_rate * fus_scale,
          fus_uns_resp_rate = changeto1(fus_uns_resp_rate),
          fus_suc_resp_rate = changeto1(fus_suc_resp_rate),
          fus_sample        =
            case_when(
              init_resp == 0 ~ rbinom(n * lr * lb, 1, fus_sample),
              init_resp == 1 ~ NA_integer_
            ),
          fus_resp          =
            case_when(
              init_resp  == 1    ~ NA_integer_,
              fus_sample == 0    ~ NA_integer_,
              harvest    == 1    ~ rbinom(n * lr * lb, 1, fus_suc_resp_rate),
              harvest    == 0    ~ rbinom(n * lr * lb, 1, fus_uns_resp_rate)
            )
        )
    }
    full_sim <- full_sim %>%
      select(method, pop_size, true_harvest, tidyselect::everything())
    return(full_sim)
  }

# Output =====================================================================
out <- purrr::map(1:times, survsim)
names(out) <- paste("Response sim", 1:length(out))
return (out)
}


# est() ========================================================================

est <- function(simdat, poststrat = FALSE){

  methods <- purrr::map(simdat, purrr::pluck, "method", 1)
  groups <- unlist(purrr::map(simdat, purrr::pluck, "group"))
  if (length(unique(groups)) != 2 & poststrat == TRUE) {
    poststrat <- FALSE
    message(
      paste0("Population cannot be post-stratified, it is not 'split'",
             " into two groups. ")
    )
  }
  if (typeof(poststrat) != "logical") {
    stop ("'poststrat' must be logical", call. = FALSE)
  }

  # Mandatory estimates ========================================================
  if (all(methods == "mandatory")){

    if (poststrat){
      poststrat <- NULL
      message(
        paste0("Cannot post-stratify a mand() simulation, 'poststrat = TRUE'",
               " ignored.")
      )
    }

    est_mand <- function(sim_elmt){
      thvst <- sim_elmt$true_harvest[[1]]
      ests <- sim_elmt %>%
        dplyr::group_by(resp_rate) %>%
        dplyr::summarise(
          true_harvest = thvst,
          est_harvest  = sum(init_resp, na.rm = TRUE),
          est_SE       = 0L,
          ARE          = abs((est_harvest - thvst) / thvst),
          sqer         = ((est_harvest - thvst)^2),
          .groups = "keep"
        )
      return(ests)
    }

    out <- purrr::map_dfr(simdat, est_mand) %>%
      dplyr::summarise(
        method        = methods[[1]],
        pop_size      = purrr::pluck(simdat, 1, "pop_size", 1),
        resp_bias     = NA_character_,
        true_hvst     = mean(true_harvest),
        mean_hvst_est = mean(est_harvest),
        mean_SE       = mean(est_SE),
        MARE          = mean(ARE),
        RRMSE         = sqrt(mean(sqer)) / mean(true_harvest),
        .groups = "drop"
      )

    out <- out %>%
      dplyr::mutate(resp_rate = as.character(resp_rate)) %>%
      dplyr::select(method:resp_bias, resp_rate, tidyselect::everything())
    return(out)

  } else if (all(methods == "simple")) {
    # SRS estimates ============================================================

    est_simp <- function(level, pop_dat){
      thvst <- pop_dat$true_harvest[[1]]
      N <- pop_dat$pop_size[[1]]

      pop <- pop_dat %>%
        dplyr::filter(dplyr::near(uns_resp_rate, level))

      init_resp_only <- pop %>%
        dplyr::filter(init_resp == 1)

      init_design <- survey::svydesign(ids   = ~1,
                                       probs = nrow(init_resp_only) / N,
                                       data  = init_resp_only,
                                       fpc   = ~pop_size)
      if (poststrat){
        strata <- data.frame(
          group = c(1, 0),
          Freq  = c(sum(pop$group), N - sum(pop$group))
        )
        init_design <- survey::postStratify(init_design,
                                            ~group,
                                            strata,
                                            partial = TRUE)
      }
      init_est <- survey::svytotal(~harvest, init_design)

      if ("fus_resp" %in% names(pop)){
        # If level is 1, then there was nobody to follow up with, so
        # ignore this step.
        if (level < 1){
          fus_resp_only <- pop %>%
            dplyr::filter(fus_resp == 1)

          fus_design <- survey::svydesign(ids   = ~1,
                                          probs = nrow(fus_resp_only) / N,
                                          data  = fus_resp_only,
                                          fpc   = ~pop_size)
          if (poststrat){
            fus_design <- survey::postStratify(fus_design,
                                               ~group,
                                               strata,
                                               partial = TRUE)
          }
          fus_est <- survey::svytotal(~harvest, fus_design)

          init_prop <- sum(pop$init_resp, na.rm = TRUE) / sum(pop$sample)
          # assume fus respondents make the rest of the pop:
          fus_prop  <- 1 - init_prop
          combined_est <- (init_est * init_prop) + (fus_est * fus_prop)

          combined_SE <- (init_prop * survey::SE(init_est)) +
            (fus_prop * survey::SE(fus_est))

          estout <- tibble::tibble(
            resp_rate    = as.character(level),
            resp_bias    = as.character(pop$resp_bias[[1]]),
            true_harvest = thvst,
            est_harvest  = as.vector(combined_est),
            est_SE       = as.vector(combined_SE),
            ARE          = abs((est_harvest - true_harvest) / true_harvest),
            sqer         = ((est_harvest - true_harvest)^2)
          )
        } else {
          estout <- tibble::tibble(
            resp_rate    = as.character(level),
            resp_bias    = as.character(pop$resp_bias[[1]]),
            true_harvest = thvst,
            est_harvest  = as.vector(init_est),
            est_SE       = as.vector(survey::SE(init_est)),
            ARE          = abs((est_harvest - true_harvest) / true_harvest),
            sqer         = ((est_harvest - true_harvest)^2)
          )
        }
      } else {
        # If there was no follow up survey simulated:
        estout <- tibble::tibble(
          resp_rate    = as.character(level),
          resp_bias    = as.character(pop$resp_bias[[1]]),
          true_harvest = thvst,
          est_harvest  = as.vector(init_est),
          est_SE       = as.vector(survey::SE(init_est)),
          ARE          = abs((est_harvest - true_harvest) / true_harvest),
          sqer         = ((est_harvest - true_harvest)^2)
        )
      }
      return(estout)
    }

    # Each element in 'simdat' must be split down to a single
    # level of response bias. This function does that, when it is the .f
    # argument in map()
    extractor <- function(sim_elmt){
      unq_bias <- unique(sim_elmt$resp_bias)
      pops <- vector(mode = "list", length = length(unq_bias))
      for (i in seq_along(unq_bias)){
        pops[[i]] <- dplyr::filter(sim_elmt, resp_bias == unq_bias[[i]])
      }
      return(pops)
    }

    splits <- purrr::map(simdat, extractor) %>%
      purrr::flatten()

    ests <- vector(mode = "list", length = length(splits))
    for (i in seq_along(splits)) {
      # This next line that contains unique() allows estimator() to further
      # filter data down to a single resp_rate, and therefore a single
      # population to create estimates from.
      ests[[i]] <- unique(splits[[i]]$uns_resp_rate) %>%
        purrr::map_dfr(est_simp, splits[[i]])
    }
    out <- ests %>%
      dplyr::bind_rows() %>%
      dplyr::group_by(resp_bias, resp_rate) %>%
      dplyr::summarise(
        method        = "simple",
        pop_size      = simdat[[1]]$pop_size[[1]],
        true_harvest  = true_harvest[[1]],
        mean_hvst_est = mean(est_harvest),
        mean_SE       = mean(est_SE),
        MARE          = mean(ARE),
        RRMSE         = sqrt(mean(sqer)) / mean(true_harvest),
        .groups = "drop"
      )

    out <- out %>%
      dplyr::select(method, pop_size, resp_bias, resp_rate,
                    tidyselect::everything())
    return(out)

  } else if (all(methods == "voluntary")){
    # Voluntary estimates ======================================================
    est_vol <- function(level, pop_dat){
      thvst <- pop_dat$true_harvest[[1]]
      N <- pop_dat$pop_size[[1]]

      pop <- pop_dat %>%
        dplyr::filter(dplyr::near(uns_resp_rate, level))

      init_resp_only <- pop %>%
        dplyr::filter(init_resp == 1)

      init_design <- survey::svydesign(ids   = ~1,
                                       probs = nrow(init_resp_only) / N,
                                       data  = init_resp_only,
                                       fpc   = ~pop_size)
      if (poststrat){
        strata <- data.frame(
          group = c(1, 0),
          Freq  = c(sum(pop$group), N - sum(pop$group))
        )
        init_design <- survey::postStratify(init_design,
                                            ~group,
                                            strata,
                                            partial = TRUE)
      }
      init_est <- survey::svytotal(~harvest, init_design)

      if ("fus_resp" %in% names(pop)){
        # if response level was already 1, there is nobody to follow up with,
        # so skip this step:
        if (level < 1){
          fus_resp_only <- pop %>%
            dplyr::filter(fus_resp == 1)

          fus_design <- survey::svydesign(ids   = ~1,
                                          probs = nrow(fus_resp_only) / N,
                                          data  = fus_resp_only,
                                          fpc   = ~pop_size)
          if (poststrat){
            fus_design <- survey::postStratify(fus_design,
                                               ~group,
                                               strata,
                                               partial = TRUE)
          }
          fus_est <- survey::svytotal(~harvest, fus_design)

          # The prop. of respondents to self-report IS the proportion
          # of the population who reported:
          init_prop <- mean(pop$init_resp, na.rm = TRUE)
          # assume fus respondents reflect the rest of the pop.
          fus_prop  <- 1 - init_prop
          combined_est <- (init_est * init_prop) + (fus_est * fus_prop)

          combined_SE <- (init_prop * survey::SE(init_est)) +
            (fus_prop  * survey::SE(fus_est))

          estout <- tibble::tibble(
            resp_rate    = as.character(level),
            resp_bias    = as.character(pop$resp_bias[[1]]),
            true_harvest = thvst,
            est_harvest  = as.vector(combined_est),
            est_SE       = as.vector(combined_SE),
            ARE          = abs((est_harvest - true_harvest) / true_harvest),
            sqer         = ((est_harvest - true_harvest)^2)
          )
        } else {
          estout <- tibble::tibble(
            resp_rate    = as.character(level),
            resp_bias    = as.character(pop$resp_bias[[1]]),
            true_harvest = thvst,
            est_harvest  = as.vector(init_est),
            est_SE       = as.vector(survey::SE(init_est)),
            ARE          = abs((est_harvest - true_harvest) / true_harvest),
            sqer         = ((est_harvest - true_harvest)^2)
          )
        }
      } else {
        # If there was no follow up survey simulated:
        estout <- tibble::tibble(
          resp_rate    = as.character(level),
          resp_bias    = as.character(pop$resp_bias[[1]]),
          true_harvest = thvst,
          est_harvest  = as.vector(init_est),
          est_SE       = as.vector(survey::SE(init_est)),
          ARE          = abs((est_harvest - true_harvest) / true_harvest),
          sqer         = ((est_harvest - true_harvest)^2)
        )
      }
      return(estout)
    }

    # Each element in 'simdat' must be split down to a single
    # level of response bias. This function does that, when it is the .f
    # argument in map()
    extractor <- function(sim_elmt){
      unq_bias <- unique(sim_elmt$resp_bias)
      pops <- vector(mode = "list", length = length(unq_bias))
      for (i in seq_along(unq_bias)){
        pops[[i]] <- dplyr::filter(sim_elmt, resp_bias == unq_bias[[i]])
      }
      return(pops)
    }

    splits <- purrr::map(simdat, extractor) %>%
      purrr::flatten()

    ests <- vector(mode = "list", length = length(splits))
    for (i in seq_along(splits)) {
      # This next line that contains unique() allows estimator() to further
      # filter data down to a single resp_rate, and therefore a single
      # population to create estimates from.
      ests[[i]] <- unique(splits[[i]]$uns_resp_rate) %>%
        purrr::map_dfr(est_vol, splits[[i]])
    }

    out <- ests %>%
      dplyr::bind_rows() %>%
      dplyr::group_by(resp_bias, resp_rate) %>%
      dplyr::summarise(
        method        = "voluntary",
        pop_size      = simdat[[1]]$pop_size[[1]],
        true_harvest  = true_harvest[[1]],
        mean_hvst_est = mean(est_harvest),
        mean_SE       = mean(est_SE),
        MARE          = mean(ARE),
        RRMSE         = sqrt(mean(sqer)) / mean(true_harvest),
        .groups = "drop"
      )

    out <- out %>%
      dplyr::select(method, pop_size, resp_bias, resp_rate,
                    tidyselect::everything())
    return(out)
  } else {
    stop (paste0("'simdat' must be an unmanipulated output from either",
                 " mand(), simple(), or vol()."))
  }
}
