module extension
    implicit none
    integer,private :: fdigit = 0
contains
    ! 出力処理(Any)が全て終わった後に呼び出す
    ! value / max : % (value out of max)
    ! isflushed : Flushする予定があるときに使う. 主にプログレスバーとともに出力結果も同時に出したいときに使う. 使わないときはfalseに設定する。
    subroutine pbout(value, max, isflushed)
        integer,intent(in) :: value, max
        double precision,save :: rate = 0d0, time = 0d0
        double precision dt, dr, estimate
        integer,parameter :: digit = 50
        integer remain(2), values(8), i
        character(len=20) :: FMT
        logical,intent(in) :: isflushed
        if (isflushed) then
            write (*, *)
        end if

        ! 変数maxの桁数を調べて書式指定子の作成を行う
        write (FMT, '(a, i0, a)') "(2(i", int(log10(real(max))) + 1, ",a))"

        ! 日時の取得
        call date_and_time(values=values)
        dr = rate   ! 前に実行したrateをひとまずdrに格納する
        dt = time   ! 前に実行したtimeをひとまずdtに格納する

        ! 時間の更新 : 今日の0時からどれだけ時間が経ったか. -> 一日を超える計算は正確に表示できない
        time = ((values(5)*60d0+values(6))*60d0+values(7))*1d3+values(8)
        dt = time - dt

        ! 割合の更新
        rate = dble(value) / dble(max)
        dr = rate - dr

        ! 残り時間の計算 (milliseconds)
        estimate = (1d0 - rate) * (dt / dr)

        ! min/sec表記に変換
        remain(1) = int(estimate/6d4) ! minutes
        remain(2) = int((estimate-remain(1)*6d4)*1d-3) ! seconds

        write (*, FMT, advance='no') value, " / ", max, " ["
        do i = 1, int(digit*rate)
            write (*, '(a)', advance='no') "="
        end do
        write (*, '(a)', advance='no') ">"
        do i = int(digit*rate) + 1, digit
            write (*, '(a)', advance='no') "-"
        end do

        if (isflushed) then
            fdigit = 2 * int(log10(real(max))) + 75
        end if
        write (*, '(a, f7.2, a, i3, a, i2, a, a)', advance='no') "]", 100d0*rate, "% ", remain(1), "m", remain(2), "s", char(13)

        if (value == max) then
            write (*, *)
        end if
    end subroutine

    ! 最初の出力処理(Any)が始まる前に呼び出す
    ! pbout()でisflushedをtrueにしたときのみ必要になる。falseのときはいらない。
    subroutine pbflush()
        character(len=9) FMT
        if (fdigit == 0) then
            return
        end if

        write (FMT, '(a, i0, a)') "(", fdigit, "x, 3a)"
        write (*, FMT, advance='no') char(13), char(8), char(13)
    end subroutine
end module

program main
    use extension
    implicit none
    integer i

    do i=1,100
        if(mod(i,5).eq.0)then
            call pbflush()
            write (*, *) "#", i
            call pbout(i,100, .true.)
            call sleep(1)
        endif
    enddo
end program