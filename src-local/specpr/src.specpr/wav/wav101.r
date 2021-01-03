	subroutine wav101
	implicit integer*4 (i-n)

	include "../common/blank"
	include "../common/lbl7"
	include "../common/lbl4"
	include "../common/label1"
	include "../common/label3"
	include "../common/lblg"
	include "../common/lundefs"

	real*4 vis(120)
	
	data    vis     /
		-1.23e34,-1.23e34,0.326800,0.326800,0.326800,0.326800,
		0.326800,-1.23e34,-1.23e34,0.350500,0.350500,0.350500,
		0.350500,-1.23e34,-1.23e34,-1.23e34,0.375000,0.375000,
		0.375000,0.375000,-1.23e34,-1.23e34,0.370161,0.375236,
		0.380341,0.385475,0.390638,0.395831,0.401053,0.406305,
		0.411585,0.416895,0.422235,0.427604,0.433002,0.438429,
		0.443886,0.449372,0.454888,0.460433,0.466007,0.471611,
		0.477244,0.482906,0.488598,0.494319,0.500069,0.505849,
		0.511657,0.517496,0.523363,0.529261,0.535187,0.541143,
		0.547128,0.553142,0.559186,0.565259,0.571362,0.577493,
		0.583655,0.589845,0.596065,0.602314,0.608593,0.614900,
		0.621238,0.627604,0.640425,0.643000,0.646880,0.653364,
		0.659877,0.666420,0.672992,0.679593,0.682244,0.692884,
		0.699574,0.706292,0.713040,-1.23e34,-1.23e34,-1.23e34,
		-1.23e34,-1.23e34,0.678967,0.691550,0.704134,0.716717,
		0.729300,0.741883,0.754466,0.767049,0.779632,0.792215,
		0.804798,0.817381,0.829964,0.842547,0.855130,0.867713,
		0.880296,0.892879,0.905463,0.918046,0.930629,0.943212,
		0.955795,0.968378,0.980961,0.993544,1.006130,1.018710,
		1.031290,1.043880,1.056460,1.069040,1.081630,1.094210 /

	write(ttyout,10)

	nchans = 120
	do i = 1,120
		dataa(i) = vis(i)
	do i = 121,256
		dataa(i) = 0.00
	call wavfil(iwavfl)
	write(ititl,112)
	ihist = ititl
	call wriwav(iwavfl)
	call wrihed(iwavfl)
	itrol(2) = iwavfl
	return

10	format(" UH PGD vis wavelengths from lab501 rec 30",/)
112	format('UH PGD vis cvf wavlngths (lab501 rec 30)')
	end
