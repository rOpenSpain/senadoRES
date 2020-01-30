# fi <- tempfile()
# fi2 <- tempfile()
# f2 <- "http://www.congreso.es/portal/page/portal/Congreso/Congreso/Diputados/BusqForm?_piref73_1333155_73_1333154_1333154.next_page=/wc/fichaDiputado?idDiputado=66&idLegislatura=14"
# fs <- xml2::download_html(f2, file = fi2)
# rs <- xml2::read_xml(fi, as_html = TRUE)
# body <- xml2::xml_find_all(rs, "//body")



# Resumen de las diferentes publicaciones...
# http://www.congreso.es/portal/page/portal/Congreso/Congreso/Publicaciones/IndPub


# Senado!!
# http://www.senado.es/web/actividadparlamentaria/publicacionesoficiales/senado/boletinesoficiales/index.html


# Senadores::
# http://www.senado.es/web/relacionesciudadanos/datosabiertos/catalogodatos/index.html

# BOCG electronico a partir del 1 de enero de 2011
# diarios de sesiones y boletines oficiales, a partir de 1 de agosto de 2012,
# tendrá consideración de copia auténtica si incluye el CVE.

#  El buscador puede ir bien para sacar codigos:
# http://www.senado.es/buscador/page/senado-lst-avanzada/sensearch?q=&sc=bofi%2Cdise&sf=&sq=mssearch_fld352%3ADS&off=0&dp=20&stem=false&is=&tes=true&trel=&od=&originForm=senado-form-publicaciones&sort=&ff=&ff=msstored_sfc_camara%3ASenado&ff=msstored_sfc_camara%3ACongreso&ff=msstored_sfc_camara%3ACortes+Generales
# Cámara: CONG/DS/CORT
# Serie: C/P/CM/CI/DP/S
# http://www.congreso.es/public_oficiales/L14/CORT/BOCG/A/BOCG-14-CG-A-1.PDF
# http://www.congreso.es/public_oficiales/L14/CORT/BOCG/B/BOCG-14-CG-B-3.PDF
# http://www.congreso.es/public_oficiales/L14/CORT/BOCG/B/BOCG-14-CG-B-2.PDF
# http://www.congreso.es/public_oficiales/L14/CORT/BOCG/B/BOCG-14-CG-B-1.PDF
# http://www.congreso.es/public_oficiales/L14/SEN/BOCG/2020/BOCG_D_14_10_304.PDF
# http://www.senado.es/legis14/publicaciones/pdf/senado/ds/DS_P_14_1.PDF
# http://www.senado.es/legis13/publicaciones/pdf/senado/ds/DS_P_13_1.PDF
# http://www.senado.es/legis13/publicaciones/xml/senado/ds/DS_P_13_1.XML
# http://www.congreso.es/portal/page/portal/Congreso/Congreso/Publicaciones/DiaSes/Pleno
# http://www.congreso.es/public_oficiales/L14/CONG/DS/PL/DSCD-14-PL-2.PDF
# http://www.congreso.es/public_oficiales/L14/SEN/BOCG/2020/BOCG_D_14_10_304.PDF
# http://www.congreso.es/public_oficiales/L14/CONG/DS/CO/DSCD-13-PL-14.PDF
# http://www.congreso.es/public_oficiales/L14/CONG/DS/CO/DSCD-13-PL-15.PDF
# http://www.congreso.es/public_oficiales/L14/CONG/DS/CO/DSCD-14-PL-1.PDF
# http://www.congreso.es/public_oficiales/L14/CONG/DS/CO/DSCD-14-PL-1-C1.PDF
# http://www.congreso.es/l14p/e0/e_0001088_n_000_2601.pdf



# http://www.senado.es/web/ficopendataservlet?tipoFich=14&legis=14#
