(use-package symon
  :if window-system
  :ensure t
  :config (progn
            (custom-set-variables
             '(symon-monitors '(symon-current-time-monitor
                                symon-linux-battery-monitor
                                symon-linux-cpu-monitor
                                symon-linux-memory-monitor
                                symon-linux-network-rx-monitor
                                symon-linux-network-tx-monitor))
             '(symon-delay 1)
             '(symon-refresh-rate 0.5)
             '(symon-history-size 25)
             '(symon-sparkline-type 'bounded))
            (symon-mode)))

(provide 'j-symon)
