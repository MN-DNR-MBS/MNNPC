using VegSci
using CSV
using XLSX
using DataFrames
using NamedArrays

base_fp = joinpath("C:/Users/zekmar/OneDrive - UKCEH/Projects/MNNPC")
mnnpc_dd_fp = joinpath(base_fp, "floristic_table_development_data_20251224.csv")

mnnpc_dd = CSV.read(mnnpc_dd_fp, DataFrame)

# Unique ECS sections
ecs_sections = String.(unique(mnnpc_dd.ecs_section))

ecs_sections = filter(x -> x != "NA", ecs_sections)

print(ecs_sections)

# Establish function to calculate fidelity metrics and write an xlsx file
function produce_fidelity_metrics(;data::DataFrame, sections::Vector{String}, output_name::String)

    # data = mnnpc_dd
    # sections = ["MOP"]
    # output_name = "statewide"

    data_use = filter(x -> x.ecs_section in sections, data)

    mnnpc_classes = sort(String.(unique(data_use.npc_class)))

    data_use = data_use[:, [:npc_class, :Quadrat, :Species, :Cover]]

    data_use = unstack(data_use, [:Quadrat, :npc_class], :Species, :Cover, fill = 0)

    releves_df = select(data_use, Not([:npc_class, :Quadrat]))

    releves_mat = NamedArrays.NamedArray(Array(releves_df); names = tuple(string.(data_use.Quadrat), names(releves_df)), dimnames = tuple("npc_class", "Species"))

    class_df = unique(data_use[:, [:npc_class, :Quadrat]])

    classes_dict = Dict

    for i in mnnpc_classes

        classes = class_df[class_df[:, :npc_class] .== i, :Quadrat]
        classes_i_dict = Dict(String.(i) => string.(classes))
        classes_dict = merge(classes_dict, classes_i_dict)
        
    end

    # VegSci.check_cluster_releveID_duplicates(classes_dict)

    # Define function to calculate and return all fidelity values
    calc_all_fidelity_values = function(rm, cd)

        fidelity_values_u = VegSci.u_fidelity(rm, cd)
        fidelity_values_phi = VegSci.phi_fidelity(rm, cd)
        fidelity_values_chisq = VegSci.chisq_fidelity(rm, cd)
        fidelity_values_G = VegSci.G_fidelity(rm, cd)
        fidelity_values_indval = VegSci.indval_fidelity(rm, cd)

        fidelity_values_u_df = rename(stack(VegSci.nm_to_df(fidelity_values_u)), ["A" => "npc_class", "variable" => "species", "value" => "u"])
        fidelity_values_phi_df = rename(stack(VegSci.nm_to_df(fidelity_values_phi)), ["A" => "npc_class", "variable" => "species", "value" => "phi"])
        fidelity_values_chisq_df = rename(stack(VegSci.nm_to_df(fidelity_values_chisq)), ["A" => "npc_class", "variable" => "species", "value" => "chisq"])
        fidelity_values_G_df = rename(stack(VegSci.nm_to_df(fidelity_values_G)), ["A" => "npc_class", "variable" => "species", "value" => "g"])
        fidelity_values_indval_df = rename(stack(VegSci.nm_to_df(fidelity_values_indval)), ["A" => "npc_class", "variable" => "species", "value" => "indval"])

        fidelity_values_df = innerjoin(fidelity_values_u_df, fidelity_values_phi_df, fidelity_values_chisq_df, fidelity_values_G_df, fidelity_values_indval_df, on = [:npc_class, :species])

        return fidelity_values_df
        
    end

    # Fidelity values across all systems and classes
    all_classes_fidelity_values_df = calc_all_fidelity_values(releves_mat, classes_dict)
    insertcols!(all_classes_fidelity_values_df, 1, :system => String.("All"))

    sort!(all_classes_fidelity_values_df, [:npc_class])

    # Check length 
    size(unique(all_classes_fidelity_values_df[:, [:npc_class, :species]]))[1] == nrow(all_classes_fidelity_values_df)

    # Fidelity values within classes by system
    systems = sort(unique(SubString.(class_df.npc_class, 1, 2)))

    fidelity_values_dict = Dict

    for system in systems

        classes_sys = mnnpc_classes[startswith.(mnnpc_classes, system)]
        cd_sys = filter(((k,v),) -> k in classes_sys, classes_dict)
        rm_sys_names = reduce(vcat, collect(values(cd_sys)))
        rm_sys = releves_mat[rm_sys_names, :]
        
        fidelity_values_df = calc_all_fidelity_values(rm_sys, cd_sys)
        insertcols!(fidelity_values_df, 1, :system => String.(system))

        fidelity_values_dict_sys = Dict(system => fidelity_values_df)
        fidelity_values_dict = merge(fidelity_values_dict, fidelity_values_dict_sys)

    end

    fidelity_values_dict["All"] = all_classes_fidelity_values_df

    fidelity_values_all_df = reduce(vcat, values(fidelity_values_dict))

    insertcols!(fidelity_values_all_df, 1, :ecs_section => output_name)

    return(fidelity_values_all_df)
    
end

mnnpc_fidelity_metrics_statewide = produce_fidelity_metrics(data = mnnpc_dd, sections = ecs_sections, output_name = "statewide")

mnnpc_fidelity_metrics_all = DataFrame()

for section in ecs_sections

    mnnpc_fidelity_metrics_section = produce_fidelity_metrics(data = mnnpc_dd, sections = [section], output_name = section)

    mnnpc_fidelity_metrics_all = vcat(mnnpc_fidelity_metrics_all, mnnpc_fidelity_metrics_section)

end

mnnpc_fidelity_metrics = vcat(mnnpc_fidelity_metrics_statewide, mnnpc_fidelity_metrics_all)

CSV.write(joinpath(base_fp, "mnnpc_fidelity_metrics.csv"), mnnpc_fidelity_metrics)