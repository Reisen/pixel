import React         from 'react';
import IconButton    from '../../../../components/IconButton';
import { range, lt } from 'ramda';
import styles        from './Pager.module.css';

interface Props {
    page: number;
    pageCount: number;
    setPage: (page: number) => void;
}

const renderButtons = (page: number, setPage: (page: number) => void) =>
    range(page - 3, page + 3)
        .filter(lt(0))
        .map(v =>
            <IconButton
                key={v.toString()}
                letter={v.toString()}
                active={v === page}
                onClick={() => setPage(v)}
            />
        );

const Pager = (props: Props) => {
    return (
        <div className={styles.Pager}>
            <div className={styles.LeftIcons}>
                <IconButton
                    tooltip="Toggle Infinite Scroll"
                    icon="infinite"
                    onClick={() => props.setPage(props.page - 1)}
                />

                <IconButton
                    tooltip="Change Grid Size"
                    icon="layout"
                    onClick={() => props.setPage(props.page - 1)}
                />

                <IconButton
                    tooltip="Toggle NSFW Filter"
                    icon="business-man-alt-1"
                    onClick={() => props.setPage(props.page - 1)}
                />
            </div>

            <div className={styles.RightIcons}>
                <div className={styles.IconRow}>
                    { renderButtons(props.page, props.setPage) }
                </div>

                <div className={styles.IconRow}>
                    <IconButton icon="simple-left" onClick={() => props.setPage(Math.max(1, props.page - 1))} />
                    <IconButton icon="simple-right" onClick={() => props.setPage(props.page + 1)} />
                </div>
            </div>
        </div>
    );
};

export default Pager;
