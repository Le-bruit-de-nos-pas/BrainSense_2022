function  plotCrossCorrVar( obj, varargin )
% PLOTCROSSCORRVAR Plot cross-correlation and cross-variance for LFP online streaming mode
%
% Syntax:
%   PLOTCROSSCORRVAR( obj, varargin );
%
% Input parameters:
%    * obj - object containg data
%    * ax (optional) - axis where you want to plot
%    * data_type (optional) - type of input data (raw, ecg cleaned or filtered)
%    * rec (optional) - recording index
%
% Example:
%   PLOTCROSSCORRVAR( obj );
%   PLOTCROSSCORRVAR( obj, ax, data_type, rec );
%
% Available at: https://github.com/NCN-Lab/DBScope
% For referencing, please use: Andreia M. Oliveira, Eduardo Carvalho, Beatriz Barros, Carolina Soares, Manuel Ferreira-Pinto, Rui Vaz, Paulo Aguiar, DBScope: 
% a versatile computational toolbox for the visualization and analysis of sensing data from Deep Brain Stimulation, doi: 10.1101/2023.07.23.23292136.
%
% Andreia M. Oliveira, Eduardo Carvalho, Beatriz Barros & Paulo Aguiar - NCN
% INEB/i3S 2022
% pauloaguiar@i3s.up.pt
% -----------------------------------------------------------------------

channel_names  = obj.streaming_parameters.time_domain.channel_names;
sampling_freq_Hz = obj.streaming_parameters.time_domain.fs;

switch nargin
    case 4
        ax          = varargin{1};
        data_type   = varargin{2};
        record      = varargin{3};

        switch data_type
            case 'Raw'
                LFP_ordered = obj.streaming_parameters.time_domain.data;
            case 'Latest Filtered'
                LFP_ordered = obj.streaming_parameters.filtered_data.data{end};
            case 'ECG Cleaned'
                LFP_ordered = obj.streaming_parameters.time_domain.ecg_clean;
        end

        LFP_aux = {};
        for d = 1:numel(LFP_ordered{record}(1,:))
            LFP_aux{end+1} = LFP_ordered{record}(:,d);
        end

        if numel(LFP_ordered{record}(1,:)) == 2

            [c5,lags5] = xcov( LFP_aux{1}, LFP_aux{2}, 'unbiased'  );
            [c6,lags6]= xcorr ( LFP_aux{1}, LFP_aux{2}, 'unbiased'  );

            cla(ax(1), 'reset');
            plot( ax(1), lags5./sampling_freq_Hz,c5 )
            title( ax(1), 'Cross-covariance LFP' )
            ylabel( ax(1), 'Amplitude [\muVp]' )
            xlabel( ax(1), 'Time [sec]' )

            cla(ax(2), 'reset');
            plot( ax(2), lags6./sampling_freq_Hz,c6 )
            title( ax(2), 'Cross-correlation LFP' )
            ylabel( ax(2), 'Amplitude [\muVp]' )
            xlabel( ax(2), 'Time [sec]' )

            %Correlation coefficients
            corelation_coef = corrcoef( LFP_aux{1}, LFP_aux{2} );

        else
            disp('Only one hemispheres available')

        end

    case 1

        % Check if data is filtered and select data
        if isempty (obj.streaming_parameters.filtered_data.data)
            disp('Data is not filtered')
            answer = questdlg('Which data do you want to visualize?', ...
                '', ...
                'Raw','ECG clean data','ECG clean data');
            switch answer
                case 'Raw'
                    LFP_ordered = obj.streaming_parameters.time_domain.data;
                case 'ECG clean data'
                    LFP_ordered = obj.streaming_parameters.time_domain.ecg_clean;
            end


            for c = 1:numel(LFP_ordered)
                if length(LFP_ordered{c}(1,:)) == 2

                    % Apply cross correlation and cross-covariance
                    [c5,lags5] = xcov( LFP_ordered{c}(:,1) , LFP_ordered{c}(:,2), 'unbiased'  );
                    [c6,lags6]= xcorr ( LFP_ordered{c}(:,1) , LFP_ordered{c}(:,2), 'unbiased'   );

                    % Plot results
                    fig = figure;
                    new_position = [16, 48, 1425, 727];
                    set(fig, 'position', new_position)
                    subplot (1,2,1)
                    plot( lags5./sampling_freq_Hz,c5 )
                    title('Cross-covariance LFP channels: ' )
                    subtitle( [channel_names{c}(:,1) ' & '  channel_names{c}(:,2) ], 'Interpreter', 'none');
                    ylabel( 'Amplitude [\muVp]' )
                    xlabel( 'Time [sec]' )
                    subplot(1,2,2)
                    plot( lags6./sampling_freq_Hz,c6 )
                    title('Cross-correlation LFP channels: ' )
                    subtitle( [channel_names{c}(:,1) ' & '  channel_names{c}(:,2) ], 'Interpreter', 'none');
                    ylabel( 'Amplitude [\muVp]' )
                    xlabel( 'Time [sec]' )
                    disp( [ 'channel 1: ' char(obj.streaming_parameters.time_domain.channel_names{c}(:,1)),...
                        'channel 2: ' char(obj.streaming_parameters.time_domain.channel_names{c}(:,2))])

                    %Correlation coefficients
                    corelation_coef = corrcoef( LFP_ordered{c}(:,1) , LFP_ordered{c}(:,2)  );

                elseif length(LFP_ordered{c}(1,:)) == 1

                    disp('Only one hemispheres available')

                end
            end
        else
            disp('Data is filtered')
            answer = questdlg('Which data do you want to visualize?', ...
                '', ...
                'Raw/ECG clean data','Latest Filtered','Latest Filtered');
            % Handle response
            switch answer
                case'Raw/ECG clean data'
                    answer = questdlg('Which data do you want to visualize?', ...
                        '', ...
                        'Raw','ECG clean data','ECG clean data');
                    switch answer
                        case 'Raw'
                            LFP_ordered = obj.streaming_parameters.time_domain.data;
                        case 'ECG clean data'
                            LFP_ordered = obj.streaming_parameters.time_domain.ecg_clean;
                    end

                    for c = 1:numel(LFP_ordered)
                        if length(LFP_ordered{c}(1,:)) == 2

                            % Apply cross correlation and cross-covariance
                            [c5,lags5] = xcov( LFP_ordered{c}(:,1) , LFP_ordered{c}(:,2), 'unbiased'  );
                            [c6,lags6]= xcorr ( LFP_ordered{c}(:,1) , LFP_ordered{c}(:,2), 'unbiased'   );

                            % Plot results
                            fig = figure;
                            new_position = [16, 48, 1425, 727];
                            set(fig, 'position', new_position)
                            subplot (1,2,1)
                            plot( lags5./sampling_freq_Hz,c5 )
                            title('Cross-covariance LFP channels: ' )
                            subtitle( [channel_names{c}(:,1) ' & '  channel_names{c}(:,2) ], 'Interpreter', 'none');
                            ylabel( 'Amplitude [\muVp]' )
                            xlabel( 'Time [sec]' )
                            subplot(1,2,2)
                            plot( lags6./sampling_freq_Hz,c6 )
                            title('Cross-correlation LFP channels: ' )
                            subtitle( [channel_names{c}(:,1) ' & '  channel_names{c}(:,2) ], 'Interpreter', 'none');
                            ylabel( 'Amplitude [\muVp]' )
                            xlabel( 'Time [sec]' )
                            disp( [ 'channel 1: ' char(obj.streaming_parameters.time_domain.channel_names{c}(:,1)),...
                                'channel 2: ' char(obj.streaming_parameters.time_domain.channel_names{c}(:,2))])

                            %Correlation coefficients
                            corelation_coef = corrcoef( LFP_ordered{c}(:,1) , LFP_ordered{c}(:,2)  );

                        elseif length(LFP_ordered{c}(1,:)) == 1

                            disp('Only one hemispheres available')

                        end
                    end

                case 'Latest Filtered'
                    LFP_ordered = obj.streaming_parameters.filtered_data.data{end};

                    for c = 1:numel(LFP_ordered)
                        if length(LFP_ordered{c}(1,:)) == 2

                            % Apply cross correlation and cross-covariance
                            [c5,lags5] = xcov( LFP_ordered{c}(:,1) , LFP_ordered{c}(:,2), 'unbiased'  );
                            [c6,lags6]= xcorr ( LFP_ordered{c}(:,1) , LFP_ordered{c}(:,2, 'unbiased' )  );

                            % Plot results
                            fig = figure;
                            new_position = [16, 48, 1425, 727];
                            set(fig, 'position', new_position)
                            subplot (1,2,1)
                            plot( lags5./sampling_freq_Hz,c5 )
                            title('Cross-covariance LFP channels: ' )
                            subtitle( [channel_names{c}(:,1) ' & '  channel_names{c}(:,2) ], 'Interpreter', 'none');
                            ylabel( 'Amplitude [\muVp]' )
                            xlabel( 'Time [sec]' )
                            subplot(1,2,2)
                            plot( lags6./sampling_freq_Hz,c6 )
                            title('Cross-correlation LFP channels: ' )
                            subtitle( [channel_names{c}(:,1) ' & '  channel_names{c}(:,2) ], 'Interpreter', 'none');
                            ylabel( 'Amplitude [\muVp]' )
                            xlabel( 'Time [sec]' )
                            disp( [ 'channel 1: ' char(obj.streaming_parameters.time_domain.channel_names{c}(:,1)),...
                                'channel 2: ' char(obj.streaming_parameters.time_domain.channel_names{c}(:,2))])

                            % Correlation coefficients
                            corelation_coef = corrcoef( LFP_ordered{c}(:,1) , LFP_ordered{c}(:,2)  );

                        elseif length(LFP_ordered{c}(1,:)) == 1

                            disp('Only one hemispheres available')

                        end
                    end
            end
        end

end
end
