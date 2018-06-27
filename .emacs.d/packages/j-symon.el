(use-package symon
  :if window-system
  :ensure t
  :config (progn
            (custom-set-variables
             '(symon-monitors
               (cond
                ((eq system-type 'gnu/linux)
                 '(symon-current-time-monitor
                   symon-linux-battery-monitor
                   symon-linux-cpu-monitor
                   symon-linux-memory-monitor
                   symon-linux-network-rx-monitor
                   symon-linux-network-tx-monitor))
                ((eq system-type 'darwin)
                 '(symon-current-time-monitor
                   symon-darwin-battery-monitor
                   symon-darwin-cpu-monitor
                   symon-darwin-memory-monitor
                   symon-darwin-network-rx-monitor
                   symon-darwin-network-tx-monitor))
                (t (error "Unsupported OS for Symon Package: `%s'" (symbol-name system-type)))
                )
               )
             '(symon-delay 1)
             '(symon-refresh-rate 0.5)
             '(symon-history-size 25)
             '(symon-sparkline-type 'bounded))
            (symon-mode)))

(provide 'j-symon)
