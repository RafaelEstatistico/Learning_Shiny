
libname rsetor "D:\BNDES_presidente\Setor";
/**************************************** Exporta��o ******************************************/
data export; 
retain ano diretox grupo_n setor uf cnae20 empr_anos pot potec rot PO_TGrau Tempo_Estudo_Me 
       IDADE_Me msal ren_media_me share vdir;
set rsetor.painel_match;

     if grupo = 0 then grupo_n = "Industria Extrativa                ";
else if grupo = 1 then grupo_n = "Baixa Intensidade Tecnol�gica";
else if grupo = 2 then grupo_n = "M�dia-Baixa Intensidade Tecnol�gica";
else if grupo = 3 then grupo_n = "M�dia-Alta Intensidade Tecnol�gica";
else if grupo = 4 then grupo_n = "Alta Intensidade Tecnol�gica";
else if grupo = 5 then grupo_n = "KIBS Servi�os";
else if grupo = 6 then grupo_n = "N�o-KIBS Servi�os";
else if grupo = 7 then grupo_n = "Com�rcio: Varejo";
else if grupo = 8 then grupo_n = "Com�rcio: Atacado";
else if grupo = 9 then grupo_n = "Ind�stria da Constru��o Civil";

drop empresa grupo vdir2012--vindir_psi2015 vindir vindir_psi lpo_t lrot_t lpotec_t prob lshare_t matchto venc_quant
     venc_valor perd_quant perd_valor apoiox ind_PSIx indiretox regiao1-regiao5;
run;

PROC EXPORT DATA = export outfile="C:\Users\rafal\Google Drive\GitHub\Learning_Shiny\01 - BNDES\data\BNDES_DATA.xlsx" dbms=xlsx replace; 
     sheet="SHEET1"; 
run;

proc sql; select diretox,  count(distinct empresa) from rsetor.painel_match group by diretox;
