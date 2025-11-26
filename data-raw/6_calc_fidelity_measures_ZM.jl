using VegSci
using CSV
using DataFrames
using NamedArrays

base_fp = joinpath("C:/Users/zekmar/OneDrive - UKCEH/Projects/MNNPC")
mnnpc_dd_fp = joinpath(base_fp, "floristic_table_development_data_20251023.csv")

mnnpc_dd = CSV.read(mnnpc_dd_fp, DataFrame)

mnnpc_dd = mnnpc_dd[:, [:npc_class, :Quadrat, :Species, :Cover]]

mnnpc_dd = unstack(mnnpc_dd, [:Quadrat, :npc_class], :Species, :Cover, fill = 0)

releves_df = select(mnnpc_dd, Not([:npc_class, :Quadrat]))

releves_mat = NamedArrays.NamedArray(Array(releves_df); names = tuple(string.(mnnpc_dd.Quadrat), names(releves_df)), dimnames = tuple("npc_class", "Species"))

class_df = unique(mnnpc_dd[:, [:npc_class, :Quadrat]])

classes_dict = Dict

for i in unique(class_df.npc_class)

    classes = class_df[class_df[:, :npc_class] .== i, :Quadrat]
    classes_i_dict = Dict(String.(i) => string.(classes))
    classes_dict = merge(classes_dict, classes_i_dict)
    
end

VegSci.check_cluster_releveID_duplicates(classes_dict)

fidelity_values_u = VegSci.u_fidelity(releves_mat, classes_dict)
fidelity_values_phi = VegSci.phi_fidelity(releves_mat, classes_dict)
fidelity_values_chisq = VegSci.chisq_fidelity(releves_mat, classes_dict)
fidelity_values_G = VegSci.G_fidelity(releves_mat, classes_dict)
fidelity_values_indval = VegSci.indval_fidelity(releves_mat, classes_dict)

fidelity_values_u_df = rename(stack(VegSci.nm_to_df(fidelity_values_u)), ["A" => "npc_class", "variable" => "species", "value" => "u"])
fidelity_values_phi_df = rename(stack(VegSci.nm_to_df(fidelity_values_phi)), ["A" => "npc_class", "variable" => "species", "value" => "phi"])
fidelity_values_chisq_df = rename(stack(VegSci.nm_to_df(fidelity_values_chisq)), ["A" => "npc_class", "variable" => "species", "value" => "chisq"])
fidelity_values_G_df = rename(stack(VegSci.nm_to_df(fidelity_values_G)), ["A" => "npc_class", "variable" => "species", "value" => "g"])
@time fidelity_values_indval_df = rename(stack(VegSci.nm_to_df(fidelity_values_indval)), ["A" => "npc_class", "variable" => "species", "value" => "indval"])

fidelity_values_df = innerjoin(fidelity_values_u_df, fidelity_values_phi_df, fidelity_values_chisq_df, fidelity_values_G_df, fidelity_values_indval_df, on = [:npc_class, :species])

CSV.write(joinpath(base_fp, "mnnpc_fidelity_values.csv"), fidelity_values_df)