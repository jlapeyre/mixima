#!/usr/bin/perl -w

# These functions are merely shadowed via apply. Many or all of them
# need to be reimpemented because they do not reproduce all functionality,
# or because of other issues.

# This program should be rewritten in lisp at some point

# We should add more directives here that do things such as make a Maxima function
# listable when it is not, etc. (via tellsimp, maybe, or in writing the function
# definition.

# The descriptions are lists of one or two elements.
#  one element -- assume Mma name is the same as the
#        Maxima name , but capitalized.
#  two elements -- the Mma name is given explicitly

# Notes: floor and cos should perhaps be treated differently ?
#  'cos(1.0) --> number
#  'floor(2.5) --> floor(2.5)
# I guess that floor is implemented via a function and cos via
# simplification.

our @shadow_math_functions = (
    ['airy_ai','AiryAi'],
    ['airy_bi','AiryBi'],
    ['airy_dai','AiryAiPrime'],
    ['airy_dbi','AiryBiPrime'],
    ['bessel_j','BesselJ'],
    ['bessel_k','BesselK'],
    ['bessel_y','BesselY'],
    ['bessel_i','BesselI'],
    ['ceiling'],
    ['conjugate'],
    ['floor'],
    ['gamma'],
    ['log_gamma', 'LogGamma'],
    ['gamma_incomplete_regularized', 'GammaRegularized'],
    ['beta'],
    ['sin'],
    ['cos'],
    ['tan'], # not atan
    ['asin','ArcSin'],
    ['acos','ArcCos'],
    ['cosh'],
    ['sinh'],
    ['tanh'],
    ['asinh','ArcSinh'],
    ['acosh','ArcCosh'],
    ['atanh','ArcTanh'],
    ['exp'],
    ['erf'],
    ['erfc'],
    ['sqrt'],
    ['pochhammer'],
    ['legendre_p','LegendreP'],
    ['legendre_q','LegendreQ'],
    ['laguerre','LaguerreL'],
    ['hermite'],
    ['lambert_w', 'ProductLog'],
# there is also hgfred. each reproduces HypergeometricPFQ in different situations
# eg rectform(hgfred([5,6],[8], 5.7 - %i)) is useful sometimes.
# hgfred([v+1/2],[2*v+1],2*%i*z) is useful, but the same with 'hypergeometric' is not
# so the line below only works sometimes
#    ['hypergeometric', 'HypergeometricPFQ'],  in fact, now trying a function
    ['elliptic_f','EllipticF'],
    ['elliptic_e','EllipticE'],
    ['elliptic_eu','EllipticEU'],
    ['elliptic_pi','EllipticPI'],
    ['elliptic_kc','EllipticKC'],
    ['elliptic_ec','EllipticEC'],
    ['expintegral_e', 'ExpIntegralE'],
    ['zeta'],
    
    );

our @jacobi_dat = qw( sn cn dn ns sc sd nc cs cd nd ds dc );
our @shadow_jacobi;
foreach (@jacobi_dat) {
    my $maxf = "jacobi_" . $_;
    my $app = $_;
    $app =~ tr/[a-z]/[A-Z]/;
    my $mmaf = "Jacobi" . $app;
    my $imaxf = "inverse_jacobi_" . $_;
    my $immaf = "InverseJacobi" . $app;
    push @shadow_jacobi, [$maxf,$mmaf];
    push @shadow_jacobi, [$imaxf,$immaf];
}

@shadow_math_functions = (@shadow_math_functions, @shadow_jacobi);


our @shadow  = (
    ['abs'],  # math function ?
#    ['block','Module'],  # These block-like constructs don't work well here. done now in lisp
#    ['block','Block'],
#    ['block','With'],  # all different in Mmma
    ['grind','InputForm'],
    ['rest','Drop'], 
    ['evenp','EvenQ'],
    ['quit','Exit'],
    ['first'],
    ['factor'],
    ['expand'],
    ['ev', 'Evaluate'],
    ['solve'],
    ['imagpart','Im'],
    ['realpart','Re'],
    ['append','Join'],
    ['last'],
#    ['length'], # NO NOT THIS!
    ['mod'],
    ['oddp','OddQ'],
    ['numberp','NumberQ'],
    ['stringp','StringQ'],
    ['split','StringSplit',['map',1]], # map over first arg if a list. not implemented yet.
    ['primep','PrimeQ'],
    ['listp','ListQ'],
    ['integerp','IntegerQ'],
    ['f', '"+"','Plus'],
    ['f', '"*"','Times'],
    ['f', '"-"','Minus'],
    ['plot2d','Plot'],
    ['print'],
    ['rest'],
#    ['return'], this will break new code
    ['join','Riffle'],
    ['taylor','Series'],
    ['sort'],
    ['sconcat','ToString'],
    ['sreverse','StringReverse'],
#    ['sublist', 'Select'], # changed to function
    ['trigexpand','TrigExpand'], 
#    ['trigreduce','TrigReduce'],NO, not this
    ['charlist','Characters'],
    );
    

sub write_map {
    my($spec) = @_;
}

# Trying various methods to shadow the functions.
# Alias. New function and use apply.
# Should try simplify.
sub write_file {
    my ($shadow,$fname) = @_;
    open(OH,">$fname") or die "Can't write to $fname";
    my $N = (@$shadow);
    my $type = 'alias';
    print OH "/* $N functions shadowed */\n";
    foreach(@$shadow) {
	my $mmaf;
        if (@$_ == 3 and $_->[0] eq 'f') {
            shift (@$_);
#            $type = 'function';
            $type = 'function';
        }
        else {
            $type = 'alias';
        }
	my $maxf = $_->[0];
	if (@$_ == 1) {
	    $mmaf = ucfirst($maxf);
	}
	else { $mmaf = $_->[1];}
	print OH "/*> Function $mmaf $maxf */\n";
        print OH $mmaf . '([e]) := apply(\'' . $maxf . ',e);' . "\n";
#	print OH $mmaf . '([e]) := apply(\'' . $maxf . ',e);' . "\n" if $type eq 'function';
#        print OH "alias($mmaf,$maxf);\n" if $type eq 'alias';
    }
    print OH "\n";
    close(OH);
}

write_file(\@shadow,"./compatibility/shadow.mac");
write_file(\@shadow_math_functions,"./compatibility/shadow_math_functions.mac");
