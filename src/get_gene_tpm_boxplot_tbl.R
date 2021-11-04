# get_gene_tpm_boxplot_tbl.R defines a function get_gene_tpm_boxplot_tbl to
# return a tibble for generating ggplot2 boxplot.
#
# Call sequence:
#
# - docker run calls Rscript --vanilla main.R
# - ../main.R calls source("src/get_gene_tpm_boxplot_tbl.R")
#
# Defined variables:
#
# - get_gene_tpm_boxplot_tbl

# Get a tibble for generating a ggplot boxplot of a single-gene, one or more
# diseasees, and zero or more GTEx tissue(s).
#
# Args:
# - gene_tpm_tbl: a tibble of a single-gene, one or more diseasees, and zero or
#   more GTEx tissue(s), returned by get_gene_tpm_tbl.
# - spec_desc_group: a single character value with the following choices for
#   grouping biospecimen descriptors. Raise error if no (Disease, cohort) tuple
#   left after filtering.
#   - "primary_and_relapse_same_group": primary and relapse samples have the
#     same group. Remove any (Disease, cohort) tuple that has no relapse or
#     primary sample.
#   - "primary_and_relapse_different_groups": primary and relapse samples have
#     different groups. Remove any (Disease, cohort) tuple that has no relapse
#     or primary sample.
#   - "primary_only_group": for each (Disease, cohort) tuple, keep only primary
#     samples.
#   - "relapse_only_group": for each (Disease, cohort) tuple, keep only relapse
#     samples.
# - gtex_histology_group: a single character value with the following choices
#   for grouping GTEx samples into different box x-labels.
#   - "tissue_subgroup": Default. Use GTEx_tissue_subgroup column as box group.
#   - "collapse": Collapse all GTEx samples into one box group with the
#     following name, "All tissue subgroups".
# - min_n_per_box: a single numeric value of the minimum number of samples per
#   box. Default is 1. Raise error if no Disease after filtering
#
# Returns a tibble for generating a ggplot boxplot of a single-gene, one or more
# diseasees, and zero or more GTEx tissue(s).
get_gene_tpm_boxplot_tbl <- function(
  gene_tpm_tbl, spec_desc_group = "primary_and_relapse_same_group",
  gtex_histology_group = "tissue_subgroup", min_n_per_box = 1L) {

  # Check input parameters
  stopifnot(is.character(spec_desc_group))
  stopifnot(identical(length(spec_desc_group), 1L))
  stopifnot(
    spec_desc_group %in%
      c("primary_and_relapse_same_group",
        "primary_and_relapse_different_groups",
        "primary_only_group", "relapse_only_group"))

  stopifnot(is.character(gtex_histology_group))
  stopifnot(identical(length(gtex_histology_group), 1L))
  stopifnot(gtex_histology_group %in% c("tissue_subgroup", "collapse"))

  stopifnot(is.numeric(min_n_per_box))
  stopifnot(identical(length(min_n_per_box), 1L))
  stopifnot(!is.na(min_n_per_box))

  # Raise error if there is no disease sample, i.e., no data available.
  stopifnot(!all(is.na(gene_tpm_tbl$Disease)))

  # Handle GTEx tissue. Separate or collapse. Create gtex_histology_group
  # column.
  if (gtex_histology_group == "tissue_subgroup") {
    gene_tpm_tbl <- dplyr::mutate(
      gene_tpm_tbl,
      histology_group = dplyr::if_else(
        is.na(Disease), true = GTEx_tissue_subgroup, false = Disease))
  } else if (gtex_histology_group == "collapse") {
    gene_tpm_tbl <- dplyr::mutate(
      gene_tpm_tbl,
      histology_group = dplyr::if_else(
        is.na(Disease), true = "All tissue subgroups", false = Disease))
  } else {
    stop(paste0(
      "Not implemented gtex_histology_group option ", gtex_histology_group))
  }

  # Handle specimen discriptor fills. Create spec_desc_group column.
  #
  # Need to group by Diseases. One boxplot may have one or more Diseases, and
  # some Diseases may have both primary and relapse, and some Diseases may have
  # primary or relapse.

  # Helper function to get boxplot tibble for each (histology_group, cohort)
  # tuple group, which is passed into dplyr::group_modify.
  #
  # Args:
  # - hc_grp_tbl: (histology_group, cohort) tuple group tibble, which does not
  #   have histology_group or cohort column, by the design of
  #   dplyr::group_modify.
  # - grp_key_tbl: (histology_group, cohort) tuple group key, a one row tibble
  #   with histology_group and cohort columns.
  #
  # Returns a boxplot tibble with the following columns added:
  # - specimen_descriptor_fill:
  #   - "Pediatric Primary Tumors", "Pediatric Relapse Tumors", and "Pediatric
  #     Primary and Relapse Tumors" for Diseases.
  #   - "GTEx Normal Adult Tissues" for GTEx.
  # - specimen_descriptor_x_label, which is different from
  #   specimen_descriptor_fill when spec_desc_group value is
  #   "primary_and_relapse_different_groups".
  #   - "Pediatric Primary Tumors", "Pediatric Relapse Tumors", and "Pediatric
  #     Primary and Relapse Tumors" for Diseases.
  #   - "GTEx Normal Adult Tissues" for GTEx.
  # - box_sample_count: number of samples for each box.
  # - x_label_sample_count: number of samples for each x-label, which is
  #   different from box_sample_count when spec_desc_group value is
  #   "primary_and_relapse_different_groups".
  # - x_label: x-labels for each box.
  get_htg_coh_grp_bp_tbl <- function(hc_grp_tbl, grp_key_tbl) {
    stopifnot(tibble::is_tibble(hc_grp_tbl))
    stopifnot(tibble::is_tibble(grp_key_tbl))

    hc_grp_n_samples <- nrow(hc_grp_tbl)
    stopifnot(is.integer(hc_grp_n_samples))
    stopifnot(hc_grp_n_samples >= 1L)

    stopifnot(identical(nrow(grp_key_tbl), 1L))
    stopifnot(identical(colnames(grp_key_tbl), c("histology_group", "cohort")))

    uniq_spec_desc_vec <- unique(hc_grp_tbl$specimen_descriptor)
    stopifnot(length(uniq_spec_desc_vec) >= 1L)
    stopifnot(all(!is.na(uniq_spec_desc_vec)))

    # - Filter specimen descriptor.
    # - Add other columns.
    # - Filter min_n_per_box. This filtering step needs to be within this
    #   procedure. For primary and relapse, an x-label may have two boxes with
    #   different fills. Each box needs to be filtered, before determining
    #   whether primary, or relapse, or both present.
    if (grp_key_tbl$cohort == "GTEx") {
      # Handle Normal
      stopifnot(identical(uniq_spec_desc_vec, "GTEx Normal"))
      bp_tbl <- dplyr::mutate(
        hc_grp_tbl,
        specimen_descriptor_fill = "GTEx Normal Adult Tissues",
        specimen_descriptor_x_label = "GTEx Normal Adult Tissues")
    } else {
      # Handle Disease
      #
      # primary, relapse, and primary-and-relapse group strings used for
      # plotting, which can be changed at a later point.
      prm_and_rlp_plot_str <- "Pediatric Primary and Relapse Tumors"  # nolint: object_usage_linter.
      prm_plot_str <- "Pediatric Primary Tumors"  # nolint: object_usage_linter.
      rlp_plot_str <- "Pediatric Relapse Tumors"  # nolint: object_usage_linter.

      stopifnot(
        all(uniq_spec_desc_vec %in% c("Primary Tumor", "Relapse Tumor")))

      if (spec_desc_group %in% c("primary_and_relapse_same_group",
                                 "primary_and_relapse_different_groups")) {
        # If this (Disease, cohort) does not have primary or relapse sample,
        # return an empty table.
        if (!all(c("Primary Tumor", "Relapse Tumor") %in% uniq_spec_desc_vec)) {
          bp_tbl <- dplyr::mutate(
            dplyr::slice_head(hc_grp_tbl, n = 0),
            specimen_descriptor_fill = character(),
            specimen_descriptor_x_label = character())
        } else {
          # Use else if to simplify adding more options at later point.
          if (identical(spec_desc_group, "primary_and_relapse_same_group")) {
            bp_tbl <- dplyr::mutate(
              hc_grp_tbl,
              specimen_descriptor_fill = .env$prm_and_rlp_plot_str,
              specimen_descriptor_x_label = .env$prm_and_rlp_plot_str)
          } else if (identical(spec_desc_group,
                               "primary_and_relapse_different_groups")) {
            # Add specimen_descriptor_fill
            bp_tbl <- dplyr::mutate(
              hc_grp_tbl,
              specimen_descriptor_fill = dplyr::case_when(
                .data$specimen_descriptor == "Primary Tumor"
                  ~ .env$prm_plot_str,
                .data$specimen_descriptor == "Relapse Tumor"
                  ~ .env$rlp_plot_str,
                TRUE ~ NA_character_
              ),
              specimen_descriptor_x_label = .env$prm_and_rlp_plot_str
            )

            stopifnot(all(!is.na(bp_tbl$specimen_descriptor_fill)))
          } else {
            stop("Internal error. Check get_htg_coh_grp_bp_tbl function.")
          }
        }
      } else if (spec_desc_group == "primary_only_group") {
        # dplyr::mutate also works for empty table
        bp_tbl <- dplyr::mutate(
          dplyr::filter(
            hc_grp_tbl, .data$specimen_descriptor == "Primary Tumor"),
          specimen_descriptor_fill = .env$prm_plot_str,
          specimen_descriptor_x_label = .env$prm_plot_str)
      } else if (spec_desc_group == "relapse_only_group") {
        bp_tbl <- dplyr::mutate(
          dplyr::filter(
            hc_grp_tbl, .data$specimen_descriptor == "Relapse Tumor"),
          specimen_descriptor_fill = .env$rlp_plot_str,
          specimen_descriptor_x_label = .env$rlp_plot_str)
      } else {
        stop(paste0(
          "Not implemented primary_and_relapse_different_groups option ",
          spec_desc_group))
      }
    }

    # dplyr::add_count works for empty table
    bp_tbl <- dplyr::add_count(
      bp_tbl, specimen_descriptor_x_label, name = "x_label_sample_count")

    bp_tbl <- dplyr::add_count(
      bp_tbl, specimen_descriptor_x_label, specimen_descriptor_fill,
      name = "box_sample_count")

    # dplyr::mutate and paste0 work for empty table
    bp_tbl <- dplyr::mutate(
      bp_tbl,
      x_label = paste0(
        .env$grp_key_tbl$histology_group, " ",
        " (Dataset = ", .env$grp_key_tbl$cohort,
        ", Specimen = ", .data$specimen_descriptor_x_label,
        ", N = ", .data$x_label_sample_count, ")"))

    # Filter boxes by box_sample_count.
    if (grp_key_tbl$cohort != "GTEx" &&
          spec_desc_group == "primary_and_relapse_different_groups") {
      if (any(bp_tbl$box_sample_count < min_n_per_box)) {
        # Either primary or relapse has < min_n_per_box samples.
        bp_tbl <- dplyr::slice_head(bp_tbl, n = 0)
      }
    } else {
      bp_tbl <- dplyr::filter(
        bp_tbl, .data$box_sample_count >= .env$min_n_per_box)
    }

    return(bp_tbl)
  }

  # After group_modify, tbl is still grouped.
  gene_tpm_boxplot_tbl <- dplyr::ungroup(
    dplyr::group_modify(
      dplyr::group_by(gene_tpm_tbl, histology_group, cohort),
      get_htg_coh_grp_bp_tbl)
  )

  if (DEBUG) {
    stopifnot(identical(sum(is.na(gene_tpm_boxplot_tbl$cohort)), 0L))
    stopifnot(identical(sum(is.na(gene_tpm_boxplot_tbl$histology_group)), 0L))
    stopifnot(identical(
      sum(is.na(gene_tpm_boxplot_tbl$specimen_descriptor_fill)), 0L))
    stopifnot(identical(
      sum(is.na(gene_tpm_boxplot_tbl$specimen_descriptor_x_label)), 0L))
    stopifnot(identical(sum(is.na(gene_tpm_boxplot_tbl$box_sample_count)), 0L))
    stopifnot(identical(
      sum(is.na(gene_tpm_boxplot_tbl$x_label_sample_count)), 0L))
    stopifnot(identical(sum(is.na(gene_tpm_boxplot_tbl$x_label)), 0L))
    stopifnot(all(gene_tpm_boxplot_tbl$box_sample_count >= min_n_per_box))

    # xor evaluates to TRUE if two values are different
    stopifnot(identical(
      sum(!xor(
        is.na(gene_tpm_boxplot_tbl$Disease),
        is.na(gene_tpm_boxplot_tbl$GTEx_tissue_subgroup))),
      0L))
  }

  # Raise error if there is no disease sample, i.e., no data available.
  stopifnot(!all(is.na(gene_tpm_boxplot_tbl$Disease)))

  # If is.na(Disease), sample_type is normal. If !is.na(Disease), sample_type is
  # disease.
  #
  # sample_type is used to explicitly set x-label orders, i.e., disease is
  # ordered prior to normal.
  gene_tpm_boxplot_tbl <- dplyr::mutate(
    gene_tpm_boxplot_tbl,
    sample_type = dplyr::if_else(
      is.na(Disease), true = "normal", false = "disease"))

  # Order x-labels
  xlabel_levels <- dplyr::arrange(
    dplyr::distinct(
      dplyr::select(
        gene_tpm_boxplot_tbl, sample_type, histology_group, cohort, x_label),
      x_label, .keep_all = TRUE),
    sample_type, histology_group, cohort)$x_label

  if (DEBUG) {
    stopifnot(identical(
      nrow(dplyr::distinct(
        dplyr::select(
          gene_tpm_boxplot_tbl, sample_type, histology_group, cohort))),
      nrow(dplyr::distinct(
        dplyr::select(
          gene_tpm_boxplot_tbl, sample_type, histology_group, cohort,
          x_label)))
    ))

    stopifnot(identical(
      nrow(dplyr::distinct(
        dplyr::select(
          gene_tpm_boxplot_tbl, sample_type, histology_group, cohort))),
      length(unique(gene_tpm_boxplot_tbl$x_label))
    ))

    stopifnot(identical(
      length(xlabel_levels),
      length(unique(xlabel_levels))
    ))
    stopifnot(identical(
      length(xlabel_levels),
      length(unique(gene_tpm_boxplot_tbl$x_label))
    ))
  }

  gene_tpm_boxplot_tbl$x_label <- factor(
    gene_tpm_boxplot_tbl$x_label, levels = xlabel_levels)

  return(gene_tpm_boxplot_tbl)
}
