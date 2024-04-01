% Uso del método de Horner o division sintética
% Dado un polinomio P(x) y un valor 'c', se calcula P(c)
function fc = Metodo_Horner[a, c]:
  % Datos
  %    a[n] := vector de tamaño 'n' con los coeficientes de P(x)
  %    c    := valor c que se desea evaluar
  % Resultado
  %    b[0] := Valor de P(c)

  % Inicialización
  n = length(a)
  b = zeros(n)
  % Algoritmo
  b(n) = a(n)
  for k = n-1:-1:0
    b(k) = a(k) + c*b(k+1)
  endfor
endfunction