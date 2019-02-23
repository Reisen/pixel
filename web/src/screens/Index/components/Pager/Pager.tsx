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
                letter={v.toString()}
                active={v === page}
                onClick={() => setPage(v)}
            />
        );

const Pager = (props: Props) => {
    return (
        <div className={styles.Pager}>
            <div className={styles.LeftIcons}>
                <IconButton icon="pixels" onClick={() => props.setPage(props.page - 1)} />
                <IconButton icon="transparent" onClick={() => props.setPage(props.page - 1)} />
                <IconButton icon="man-in-glasses" onClick={() => props.setPage(props.page - 1)} />
            </div>

            <div className={styles.RightIcons}>
                <div className={styles.IconRow}>
                    { renderButtons(props.page, props.setPage) }
                </div>

                <div className={styles.IconRow}>
                    <IconButton icon="simple-left" onClick={() => props.setPage(props.page - 1)} />
                    <IconButton icon="simple-right" onClick={() => props.setPage(props.page + 1)} />
                </div>
            </div>
        </div>
    );
};

export default Pager;
