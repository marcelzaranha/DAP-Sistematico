# -*- coding: utf-8 -*-

import pandas as pd
import MySQLdb
import datetime as dt

################

start_dt = dt.datetime(2000,1,1)
end_dt   = dt.datetime.now()

lista_top5 = ["c", "m", "l"]
lista_calculo = ["desvio padrao", "media", "mediana"]
################

conn = MySQLdb.connect("192.168.180.249", "root", "marisco")

for top5 in lista_top5:
    for calculo in lista_calculo:

            # le do banco
            query = "select data, expec_para, valor from ec_focus.exp_inflacao where top5='{0}' and calculo='{1}' and periodicidade='mensal' and indice='ipca'".format(top5, calculo)
            df = pd.read_sql(query, conn, parse_dates="data")

            # parse expec_para (primeiro dia do mes para representar melhor)
            df["expec_para"] = pd.to_datetime(df["expec_para"], format="%Y-%m")
            # df["expec_para"] = df["expec_para"] + pd.offsets.MonthEnd(n=0) - pd.offsets.MonthBegin(n=1)

            # coluna delta_months
            df["delta"] = (df["expec_para"].dt.year - df["data"].dt.year)*12 + (df["expec_para"].dt.month - df["data"].dt.month)

            # pivot
            df = df.pivot(index="data", columns="delta", values="valor")

            # sort
            df.sort_index(axis=0, ascending=True, inplace=True)
            df.sort_index(axis=1, ascending=True, inplace=True)

            # csv
            outname = "{0}_{1}.csv".format(calculo, top5)
            df.to_csv(outname)


conn.close()