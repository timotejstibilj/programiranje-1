def f(razlika, dolzina):
    def stevilo_zaporedij(razlika, dolzina, zadnji_clen):
        if dolzina == 1:
            return 1
        else:
            moznosti = [
                stevilo_zaporedij(razlika, dolzina - 1, x)
                for x in range(zadnji_clen - razlika, zadnji_clen + razlika + 1)
                if x >= 0
            ]
        return sum(moznosti)

    return stevilo_zaporedij(razlika, dolzina, 0)