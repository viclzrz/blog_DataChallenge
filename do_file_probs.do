clear all
* abrir base de datos
use "/Users/vicentelopez/Desktop/Econometria/DataChallenge/blog_DataChallenge/bd_clean.dta"
* modificar algunas variables
gen mujer = 0
replace mujer = 1 if sexo == 2

gen salud_buena = 0
replace salud_buena = 1 if salud==2

gen salud_mb = 0
replace salud_mb = 1 if salud==3

gen fam_muerte_covid_d = 0
replace fam_muerte_covid_d = 1 if fam_muerte_covid == 2

gen evento_social_15d_d = 0
replace evento_social_15d_d = 1 if evento_social_15d == 2

gen deporte_d = 0
replace deporte_d = 1 if deporte == 2

* modelo principal
oprobit opinion mujer salud_buena salud_mb prob_contagio_3m fam_muerte_covid_d recomendaciones evento_social_15d_d deporte_d, vce(robust)
* modelo logit ordenado adiciona
ologit opinion mujer salud_buena salud_mb prob_contagio_3m fam_muerte_covid_d recomendaciones evento_social_15d_d deporte_d, vce(robust)
* calculo de efectos marginales
mfx compute, at(mean) predict(outcome(1))
mfx compute, at(mean) predict(outcome(2))
mfx compute, at(mean) predict(outcome(3))

* graficas de prediccion en probabilidad
** sexo y prob contagio
quietly oprobit opinion sexo##c.prob_contagio_3m, vce(robust)
margins sexo, at(prob_contagio_3m=(1(1)5))
marginsplot, by(sexo)
** sexo y calif recomendaciones
quietly oprobit opinion sexo##c.recomendaciones, vce(robust)
margins sexo, at(recomendaciones=(1(1)5))
marginsplot, by(sexo)
** sexo y estado salud
quietly oprobit opinion sexo##salud, vce(robust)
margins sexo#salud
marginsplot, by(sexo)
** sexo y muerte p/COVID
quietly oprobit opinion sexo##fam_muerte_covid_d, vce(robust)
margins sexo#fam_muerte_covid_d
marginsplot, by(sexo)
** sexo y asistencia a evento social
quietly oprobit opinion sexo##evento_social_15d_d, vce(robust)
margins sexo#evento_social_15d_d
marginsplot, by(sexo)
** sexo y aficionado a deporte
quietly oprobit opinion sexo##deporte_d, vce(robust)
margins sexo#deporte_d
marginsplot, by(sexo)
