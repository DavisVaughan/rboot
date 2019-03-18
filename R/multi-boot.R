# Would need to support the boot_t interface working without
# requiring std_error columns

# multi_boot_rset <- function(rset_mapped, alpha = 0.05) {
#
#   core_cols <- c("lower", "estimate", "upper", "alpha")
#   arrange_with <- setdiff(colnames(rset_mapped), core_cols)
#
#   out <- dplyr::bind_rows(
#     boot_pctl_rset(rset_mapped, alpha = alpha),
#     boot_bca_rset(rset_mapped, alpha = alpha),
#     boot_t_rset(rset_mapped, alpha = alpha)
#   )
#
#   arrange_with <- setdiff(colnames(out), core_cols)
#
#   dplyr::arrange(out, !!! rlang::syms(arrange_with))
# }
